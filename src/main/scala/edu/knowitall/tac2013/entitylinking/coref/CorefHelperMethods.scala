package edu.knowitall.tac2013.entitylinking.coref
import edu.knowitall.common.Resource.using
import edu.knowitall.tac2013.entitylinking.KBPQuery
import edu.knowitall.browser.entity.EntityLinker
import edu.knowitall.tac2013.entitylinking.SolrHelper
import edu.knowitall.tac2013.entitylinking.utils.TipsterData.expandStateAbbreviation
import edu.knowitall.tac2013.entitylinking.utils.TipsterData
import java.io.File
import scala.util.matching.Regex

object CorefHelperMethods {
  
  private val stateAbbreviationPattern = """(\w+),\s([A-Za-z])\.?([A-Za-z])\.?$""".r
  
  val queryMentionMap = {
    System.err.println("Loading query to Coref String Mentions map...")
    try{
     val corefFile = getClass.getResource(KBPQuery.year.getOrElse({throw new Exception("Activate KBP Query")})+"corefStringMentions.txt").getPath()
     Some(using{scala.io.Source.fromFile(corefFile)}{ source =>
      source.getLines.map{ line =>
        line.split("\t") match{
        	case Array(qId, e  @ _*) => {(qId,for(mention <- e) yield mention)}
        	case _ => throw new RuntimeException("Error parsing coref mentions")
        }
      } toMap
     })
    }
    catch{
      case e: Exception => {
        None
      }
    }
  }
  
  val queryNamedEntityCollectionMap2011 = loadQueryNamedEntityCollectionMap("2011")
  val queryNamedEntityCollectionMap2012 = loadQueryNamedEntityCollectionMap("2012")
  private val queryNamedEntityCollectionMap = loadQueryNamedEntityCollectionMap(KBPQuery.year.get)
  
  private def loadQueryNamedEntityCollectionMap(year: String): Option[Map[String,NamedEntityCollection]] = {
    System.err.println("Loading query to Named Entities map...")
    var namedEntityFile = ""
    try{
      namedEntityFile = getClass.getResource(year+"namedEntities.txt").getPath()
    }
    catch{
      case e: Exception => {
        System.err.println("Error loading "+KBPQuery.year.getOrElse({"noyear"})+"namedEntities.txt")
        try{
          namedEntityFile = new File("./src/main/resources/edu/knowitall/tac2013/entitylinking/coref/"+KBPQuery.year.getOrElse({throw new Exception("Activate KBP Query")})+"namedEntities.txt").getPath()
        }
        catch{
          case e: Exception => {
            None
          }
        }
      }
    }
    try{
	    Some(using{scala.io.Source.fromFile(namedEntityFile)}{ 
	      source => {
	        val lines = source.getLines.toList
	        
	        val queryNamedEntityCollections = lines.sliding(4)
	        
	        queryNamedEntityCollections.map{ necLines => {
	          val firstLine = necLines(0)
	          val firstLineValues = firstLine.split("\t")
	          val qId = firstLineValues(0)
	          var qType = "None"
	          if(firstLineValues.length > 1){
	            qType = firstLineValues(1)
	          }
	          val matchingNamedEntities = firstLineValues.drop(2)
	          val organizations = necLines(1).split("\t").drop(2)
	          val locations = necLines(2).split("\t").drop(2)
	          val people = necLines(3).split("\t").drop(2)
	          (qId,new NamedEntityCollection(qId,qType,matchingNamedEntities.toList,organizations.toList,locations.toList,people.toList))
	        }
	      } toMap
	    } 
	    })
    }
    catch{
      case e: Exception => {
        None
      }
    }
  }
  
  class NamedEntityCollection(val qId:String, val qType:String,
      val matchingNamedEntities: List[String],
      val organizations: List[String],
      val locations: List[String],
      val people: List[String]){
  }
  
  def getStanfordNERType(queryId: String, year: String) = {
    val queryNamedEntityCollectionMap = year match{
      case "2011" => { Some(queryNamedEntityCollectionMap2011)}
      case "2012" => { Some(queryNamedEntityCollectionMap2012)}
      case _ => None
    }

    if(queryNamedEntityCollectionMap.get.get.get(queryId).isDefined){
      queryNamedEntityCollectionMap.get.get.get(queryId).get.qType
    }
    else{
      "None"
    }
  }
  
  
  def identifyBestEntityStringByLinkerScore(q: KBPQuery, linker: EntityLinker): String = {
     var bestCandidate = q.name
     var bestScore = 0.0
     if(queryMentionMap.isDefined){
         val map = queryMentionMap.get
	     val candidates = q.name :: map.get(q.id).get.toList
	     val uniqueCandidates = candidates.toSet.toList
	     for(uc <- uniqueCandidates){
	          print(uc + "\t")
	          val link = linker.getBestEntity(uc,q.corefSourceContext)
	          if(link.nonEmpty){
	          println(link.get.combinedScore)
		          if(link.get.combinedScore > bestScore){
		            bestCandidate =uc
		            bestScore = link.get.combinedScore
		          }
	          }
	          else{
	            println("null")
	          }
	     }
	     bestCandidate
     }
     else{
       bestCandidate
     }
  }
  
  def identifyBestEntityStringByRules(q: KBPQuery): String = {
    //check sports classifier to override location possibilities..
    val sportsSenseOption = q.sportsSense
    val couldBeLocation = sportsSenseOption match{
      case Some(true) => {false}
      case Some(false) => {true}
      case None => {true}
    }
    println(q.name + "could be location:" + couldBeLocation)
    var alternateName = q.name
    val namedEntityCollection = queryNamedEntityCollectionMap.get.get(q.id).get
    val entityType = namedEntityCollection.qType
    if(entityType != "None"){
      alternateName =
      entityType match{
        case "ORGANIZATION" => { findBestOrganizationString(q,namedEntityCollection.organizations)}
        case "LOCATION" => {
        	if(couldBeLocation){
        	  findBestLocationString(q,namedEntityCollection.locations)
        	}
        	else{
        	  q.name
        	}
        	}
        case "PERSON" => {findBestPersonString(q,namedEntityCollection.people)}
      }
    }
    alternateName match{
      case q.name => {
        alternateName = findBestOrganizationString(q,namedEntityCollection.organizations)
        if(alternateName == q.name){
          if(couldBeLocation){
            alternateName = findBestLocationString(q,namedEntityCollection.locations)
          }
        }
        if(alternateName == q.name){
          alternateName = findBestPersonString(q,namedEntityCollection.people)
        }
      }
      case _ => {}
    }
    alternateName
  }
  
  private def findBestOrganizationString(kbpQuery: KBPQuery, candidateStrings: List[String]) :String = {
    val originalString = kbpQuery.name.trim()
    
    //if the organization is an acronym
    if(originalString.forall(p => p.isUpper)){
      
      for(cs <- candidateStrings){
        val words = cs.split(" ").filter(p => {p(0).isUpper}).takeRight(originalString.length())
        var goodCandidate = true
        var index = 0
        if(words.length >= originalString.length()){
	        for(word <- words){
	          if(word(0) != originalString(index)){
	            goodCandidate = false
	          }
	          index += 1
	        }
	        if(goodCandidate){
	          val candidateWords = cs.split(" ")
	          var index = 0
	          for(cw <- candidateWords){
	            if(cw == words.head){
	              return candidateWords.slice(index,candidateWords.length) mkString " "
	            }
	            index +=1
	          }
	        }
        }
      }
    }
    
    //non caps organization, check if there is a longer string than the original
    //name with the original name as the rightmost word
    else{
      for(cs <- candidateStrings){
        val words = cs.split(" ")
        val originalWords = originalString.split(" ")
        if( (words.length > originalWords.length) &&
            ( (words.takeRight(originalWords.length).mkString(" ") == originalString) ||
              (words.take(originalWords.length).mkString(" ") == originalString)  )){
          return words mkString " "
        }
      }
    }
    
    //finally check if the original string if prefix of an organization
    for(cs <- candidateStrings){
      if(cs.toLowerCase().startsWith(originalString.toLowerCase())){
        return cs
      }
    }
    
    originalString
    
  }
  
  def locationCasing(str: String) :String ={
    println("Doing Location Casing On :" + str)
    var words = List[String]()
    for(s <- str.split(" ")){
      var newS = s
      if(!s.contains(".")){
        newS = for(c <- s) yield {
          c.toLower
        }
        newS = newS(0).toUpper + newS.tail
      }
        words = words :+ newS
      }
    words mkString " "
  }
  
  private def expandLocation(containerLocation: String): List[String] = {
    
    val containerLocationPrefix = if(!containerLocation.last.isLetter){
      containerLocation.dropRight(1)
    }
    else{
      containerLocation
    }
    var possibleExpansions = List[String]()
    
    if(containerLocationPrefix.length() > 2){
      val stateOrProvinces = TipsterData.stateOrProvinces
      for(state <- stateOrProvinces){
        if(state.startsWith(containerLocationPrefix.toLowerCase())){
          possibleExpansions = locationCasing(state) :: possibleExpansions
        }
      }
    }
    possibleExpansions.toList    
  }
  
  private def expandAbbreviation(str:String) :String = {
    val stateAbbreviationMatch = stateAbbreviationPattern.findFirstMatchIn(str)
    if(stateAbbreviationMatch.isDefined){
      val abbreviation = stateAbbreviationMatch.get.group(2).toUpperCase() + 
    		  stateAbbreviationMatch.get.group(3).toUpperCase()
      val city = stateAbbreviationMatch.get.group(1)
      val expandedStateAbbreviation = expandStateAbbreviation(abbreviation,city)
      if(expandedStateAbbreviation.isDefined){
        expandedStateAbbreviation.get
      }
      else{
       str 
      }
    }
    else{
      //check for Mass. pattern and expand if valid
      val containedLocation = str.split(",")(0).trim()
      val containerLocation = str.split(",")(1).trim()
      val expandedLocations = expandLocation(containerLocation)
      for(expandedLocation <- expandedLocations){
        if(locationContainsLocation(expandedLocation,containedLocation)){
          return (containedLocation + ", " + expandedLocation) 
        }
      }
      str
    }
  }
  private def findBestLocationString(kbpQuery: KBPQuery, candidateStrings: List[String]) :String = {
    val originalString = kbpQuery.name.trim()
    var candidates = List[String]()
    val originalWords = originalString.split(" ")
    for(cs <- candidateStrings){
      val size = cs.split(" ").length
      var index = 0
      while(index < (size-1)){
	      val words = cs.split(" ").drop(index)
	      if( (words.length > (originalWords.length +1)) &&
	          (words.take(originalWords.length).mkString(" ").toLowerCase() == originalString.toLowerCase()) &&
	          (words(originalWords.length) == ",")){
	        candidates  = candidates :+ words.take(originalWords.length).mkString(" ") + ", " + words.drop(originalWords.length+1).mkString(" ") 
	      }
	      index += 1
      }
    }
    candidates = candidates.filter(p => (p.split(" ").length < 7))
    candidates = candidates.filter(p => (isValidLocation(p)))
    if(candidates.isEmpty){
      //check to see if state is mentioned somewhere, then build a new String with
      //that state or country
      val containerMap = scala.collection.mutable.Map[String,Int]()
      for(cs <- candidateStrings){
        if(locationContainsLocation(cs,originalString)){
          if(cs != originalString){
	          if(containerMap.contains(cs)){
	              containerMap += ((cs,containerMap.get(cs).get+1))
	          }
	          else{
	              containerMap += ((cs,1))           
	          }
          }
        }
      }
      if(containerMap.isEmpty){
        //try  regular string searching instead of relying on Stanford NER
        val containedPlace = originalString
        val locationRegex = new Regex("("+originalString+"|"+originalString.toLowerCase()+"|"+originalString.toUpperCase()+"), ([A-Z][\\S]+)[\\s\\.\\?!,]")
        val sourceText = SolrHelper.getRawDoc(kbpQuery.doc)
        val candidates = scala.collection.mutable.Map[String,Int]()
        for( locationRegex(containedLoc,containerLoc) <- locationRegex.findAllMatchIn(sourceText); fullLocation = expandAbbreviation(locationCasing(containedLoc+", " +containerLoc)).split(",");
             if locationContainsLocation(fullLocation(1).trim(),fullLocation(0).trim())) {
          println("Trying to find RAW LOCATION STRING: " + fullLocation(0) +", " + fullLocation(1))
          val containerLocation = fullLocation(1).trim()
          if(candidates.contains(containerLocation)){
            candidates += ((containerLocation, 1 + candidates.get(containerLocation).get))
          }
          else{
            candidates += ((containerLocation,1))
          }
        }
        val headTuple = candidates.toMap.toList.sortBy(f => f._2).headOption
        if(headTuple.isDefined){
          containedPlace + ", "+headTuple.get._1
        }
        else{
          originalString
        }
      }
      else{
        var largestCount =0
        var largestContainer = ""
        for(containerCandidate <- containerMap){
          val container = containerCandidate._1
          val count = containerCandidate._2
          if(count > largestCount){
            largestCount = count
            largestContainer = container
          }
        }
        locationCasing(originalString +", " + largestContainer)
      }
    }
    else{
       val candidate = candidates.head
       expandAbbreviation(locationCasing(candidate))
      }
  }
  private def findBestPersonString(kbpQuery: KBPQuery, candidateStrings: List[String]) :String = {
      val originalString = kbpQuery.name.trim()
      for(cs <- candidateStrings){
        val words = cs.split(" ")
        val originalWords = originalString.split(" ")
        if( (words.length > originalWords.length) &&
        		( (words.takeRight(originalWords.length).mkString(" ") == originalString) ||
        		   (words.take(originalWords.length).mkString(" ") == originalString)) &&
        		   (words.length < 4)){
          return words mkString " "
        }
      }
      originalString
  }
  
  private def isValidLocation(locationStr: String): Boolean = {
    val placeNames = locationStr.split(",").map(f => f.trim())
    if(placeNames.length == 2){
      return ((locationContainsLocation(placeNames(1),placeNames(0))) || (!sameLocationType(placeNames(1),placeNames(0))))
    }
    else{
      return false
    }
  }
  
  private def sameLocationType(location1: String, location2: String): Boolean = {
    val cities = TipsterData.cities
    val stateOrProvinces = TipsterData.stateOrProvinces
    val countries = TipsterData.countries
    
    if(cities.contains(location1.toLowerCase()) && cities.contains(location2.toLowerCase())){
      return true
    }
    if(stateOrProvinces.contains(location1.toLowerCase()) && stateOrProvinces.contains(location2.toLowerCase())){
      return true
    }
    if(countries.contains(location1.toLowerCase()) && countries.contains(location2.toLowerCase())){
      return true
    }
    return false
  }
  
  private def locationContainsLocation(container: String, contained: String): Boolean = {
    val cities = TipsterData.cities
    val stateOrProvinces = TipsterData.stateOrProvinces
    val countries = TipsterData.countries
    val stateCityMap = TipsterData.provinceCityMap
    val countryCityMap = TipsterData.countryCityMap
    
    if(cities.contains(contained.toLowerCase())){
      if(stateOrProvinces.contains(container.toLowerCase())){
        val citySet = stateCityMap.get(locationCasing(container))
        if(citySet.isDefined){
          if(citySet.get.contains(locationCasing(contained))){
        	  return true
          }
        }
      }
      if(countries.contains(container.toLowerCase())){
        val citySet = countryCityMap.get(locationCasing(container))
        if(citySet.isDefined){
          if(citySet.get.contains(locationCasing(contained))){
            return true
          }
        }
      }
    }
    return false
  }


}