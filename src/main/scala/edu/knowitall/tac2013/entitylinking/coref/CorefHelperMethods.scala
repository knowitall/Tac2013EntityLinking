package edu.knowitall.tac2013.entitylinking.coref
import edu.knowitall.common.Resource.using
import edu.knowitall.tac2013.entitylinking.KBPQuery
import edu.knowitall.browser.entity.EntityLinker
import edu.knowitall.tac2013.entitylinking.SolrHelper
import edu.knowitall.tac2013.entitylinking.utils.TipsterData.expandStateAbbreviation
import edu.knowitall.tac2013.entitylinking.utils.TipsterData
import java.io.File
import scala.collection.mutable

object CorefHelperMethods {
  
  private val helperCache = new mutable.HashMap[String, CorefHelperMethods] with mutable.SynchronizedMap[String, CorefHelperMethods]
  
  def get(year: String) = helperCache.getOrElseUpdate(year, new CorefHelperMethods(year))
}

class CorefHelperMethods(val year: String) {
  
  private val stateAbbreviationPattern = """(\w+),\s([A-Za-z])\.?([A-Za-z])\.?$""".r
  
  val queryMentionMap = {
    System.err.println("Loading query to Coref String Mentions map...")
    try{
     val corefFile = getClass.getResource(year+"corefStringMentions.txt").getPath()
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
  
  private val queryNamedEntityCollectionMap = {
    System.err.println("Loading query to Named Entities map...")
    var namedEntityFile = ""
    try{
      namedEntityFile = getClass.getResource(year+"namedEntities.txt").getPath()
    }
    catch{
      case e: Exception => {
        try{
          namedEntityFile = new File("./src/main/resources/edu/knowitall/tac2013/entitylinking/coref/"+year+"namedEntities.txt").getPath()
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
  
  private class NamedEntityCollection(val qId:String, val qType:String,
      val matchingNamedEntities: List[String],
      val organizations: List[String],
      val locations: List[String],
      val people: List[String]){
  }
  
  def getStanfordNERType(queryId: String) = {
    if(queryNamedEntityCollectionMap.get.get(queryId).isDefined){
      queryNamedEntityCollectionMap.get.get(queryId).get.qType
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
    var alternateName = q.name
    val namedEntityCollection = queryNamedEntityCollectionMap.get.get(q.id).get
    val entityType = namedEntityCollection.qType
    if(entityType != "None"){
      alternateName =
      entityType match{
        case "ORGANIZATION" => { findBestOrganizationString(q.name.trim(),namedEntityCollection.organizations)}
        case "LOCATION" => {findBestLocationString(q.name.trim(),namedEntityCollection.locations)}
        case "PERSON" => {findBestPersonString(q.name.trim(),namedEntityCollection.people)}
      }
    }
    alternateName match{
      case q.name => {
        alternateName = findBestOrganizationString(q.name,namedEntityCollection.organizations)
        if(alternateName == q.name){
          alternateName = findBestLocationString(q.name,namedEntityCollection.locations)
        }
        if(alternateName == q.name){
          alternateName = findBestPersonString(q.name,namedEntityCollection.people)
        }
      }
      case _ => {}
    }
    alternateName
  }
  
  private def findBestOrganizationString(originalString: String, candidateStrings: List[String]) :String = {
    
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
  
  private def locationCasing(str: String) :String ={
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
      str
    }
  }
  private def findBestLocationString(originalString: String, candidateStrings: List[String]) :String = {
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
        originalString
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
  private def findBestPersonString(originalString: String, candidateStrings: List[String]) :String = {
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