package edu.knowitall.tac2013.entitylinking.coref
import edu.knowitall.common.Resource.using
import edu.knowitall.tac2013.entitylinking.KBPQuery
import edu.knowitall.browser.entity.EntityLinker
import edu.knowitall.tac2013.entitylinking.SolrHelper
import edu.knowitall.tac2013.entitylinking.utils.TipsterData.expandStateAbbreviation
import edu.knowitall.tac2013.entitylinking.utils.TipsterData
import java.io.File
import scala.util.matching.Regex
import scala.collection.mutable
import edu.knowitall.collection.immutable.Interval
import edu.knowitall.tac2013.entitylinking.utils.WikiMappingHelper
import edu.knowitall.browser.entity.EntityLink
import scala.util.control.Breaks._

object CorefHelperMethods {
  
  private val helperCache = new mutable.HashMap[String, CorefHelperMethods] with mutable.SynchronizedMap[String, CorefHelperMethods]
  
  def get(year: String) = helperCache.getOrElseUpdate(year, new CorefHelperMethods(year))
}

class CorefHelperMethods(val year: String) {
  
  private val stateAbbreviationPattern = """(\w+),\s([A-Za-z])\.?([A-Za-z])\.?$""".r
  private val stopWords = {
    val url = getClass.getResource("/edu/knowitall/tac2013/entitylinking/classifier/stopwords.txt")
    require(url != null, "Could not find stopwords.txt")
    io.Source.fromURL(url, "UTF8").getLines.flatMap(_.split(",")).map(_.toLowerCase).toSet
  }
  
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
  val corefMentionsFile =
    try {
      getClass.getResource("/edu/knowitall/tac2013/entitylinking/coref/" + year + "corefmentions.txt").getPath()
    } catch {
      case e: Exception => {
        new File("./src/main/resources/edu/knowitall/tac2013/entitylinking/coref/" + year + "corefmentions.txt").getPath()
      }
    }
  val queryToCorefMap = using(io.Source.fromFile(corefMentionsFile, "UTF8")) { source =>
    WikiMappingHelper.loadQueryToCorefMentionsMap(source.getLines)
  }
  val queryNamedEntityCollectionMap2011 = loadQueryNamedEntityCollectionMap("2011")
  val queryNamedEntityCollectionMap2012 = loadQueryNamedEntityCollectionMap("2012")
  private val queryNamedEntityCollectionMap = loadQueryNamedEntityCollectionMap(year)
  
  private def loadQueryNamedEntityCollectionMap(year: String): Option[Map[String,NamedEntityCollection]] = {
    System.err.println("Loading query to Named Entities map...")
    var namedEntityFile = 
    try{
       getClass.getResource(year + "namedEntities.txt").getPath()
    }
    catch{
      case e: Exception => {
        new File("./src/main/resources/edu/knowitall/tac2013/entitylinking/coref/" + year + "namedEntities.txt").getPath()
      }
    }

    Some(using { scala.io.Source.fromFile(namedEntityFile) } {
      source =>
        {
          val lines = source.getLines.toList

          val queryNamedEntityCollections = lines.grouped(4)

          queryNamedEntityCollections.map { necLines =>
            {
              val firstLine = necLines(0)
              val firstLineValues = firstLine.split("\t")
              val qId = firstLineValues(0)
              var qType = "None"
              if (firstLineValues.length > 1) {
                qType = firstLineValues(1)
              }
              val matchingNamedEntities = firstLineValues.drop(2)
              val organizations = necLines(1).split("\t").drop(2)
              val locations = necLines(2).split("\t").drop(2)
              val people = necLines(3).split("\t").drop(2)
              (qId, new NamedEntityCollection(qId, qType, matchingNamedEntities.toList, organizations.toList, locations.toList, people.toList))
            }
          } toMap
        }
    })
  }
  
  class NamedEntityCollection(val qId:String, val qType:String,
      val matchingNamedEntities: List[String],
      val organizations: List[String],
      val locations: List[String],
      val people: List[String]){
  }
  
  def getStanfordNERType(queryId: String, year: String) = {
    val queryNamedEntityCollectionMap = year match{
      case "2011" => { queryNamedEntityCollectionMap2011}
      case "2012" => { queryNamedEntityCollectionMap2012}
      case _ => this.queryNamedEntityCollectionMap
    }

    if(queryNamedEntityCollectionMap.get.get(queryId).isDefined){
      queryNamedEntityCollectionMap.get.get(queryId).get.qType
    }
    else{
      "None"
    }
  }
  
  
  private def getIndices(searchString: String, targetString: String): List[Interval] = {
    var intervalList = List[Interval]()
    var nextIndex = searchString.indexOf(targetString)
    while(nextIndex != -1){
      val thisInterval = Interval.closed(nextIndex,nextIndex+targetString.length()-1)
      intervalList = thisInterval :: intervalList
      nextIndex = searchString.indexOf(targetString,nextIndex+1)
    }
    intervalList.toList
  }
  
  private def searchCoreferences(kbpQuery: KBPQuery, entityType: String, namedEntityCollection: NamedEntityCollection): String = {
    val originalName = kbpQuery.name
    if(entityType == "ORGANIZATION" || entityType == "LOCATION"){
      val rawDoc = SolrHelper.getRawDoc(kbpQuery.doc,kbpQuery.year)
      val namedEntities = namedEntityCollection.locations ::: namedEntityCollection.organizations
      val corefOffsets = queryToCorefMap.get(kbpQuery.id).getOrElse(List[Interval]())
      var candidateNamedEntities = List[String]()
      for (namedEntity <- namedEntities){
        val intervals = getIndices(rawDoc.toLowerCase(),namedEntity.toLowerCase())
        for(interval <- intervals){
          for(offsets <- corefOffsets){
            if(offsets.size < 50){
	            if(offsets.contains(interval.start) && offsets.contains(interval.end)){
	              candidateNamedEntities = namedEntity.replace(" in ", ", ") :: candidateNamedEntities
	            }
            }
          }
        }
      }
      val candidate = (candidateNamedEntities.filter(p => {p.length() > originalName.length()}).filter(p => !p.contains(",")).sortBy(f => f.length()).headOption)
      if(candidate.isDefined){
          return candidate.get
      }
    }
    originalName
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
    var alternateName = q.name
    val namedEntityCollection = queryNamedEntityCollectionMap.get(q.id)
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
        case "PERSON" => {findBestPersonString(q,namedEntityCollection.people,true)}
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
          alternateName = findBestPersonString(q,namedEntityCollection.people,false)
        }
      }
      case _ => {}
    }
    if(alternateName == q.name){
      val corefString = searchCoreferences(q,entityType,namedEntityCollection)
      if(corefString.toLowerCase().contains(q.name.toLowerCase())){
        alternateName = corefString
      }
    }
    alternateName
  }
  
  private def sortCandidateStringsByProximity(kbpQuery: KBPQuery, candidateStrings: List[String]): List[String] =  {
    val rawDoc = SolrHelper.getRawDoc(kbpQuery.doc,kbpQuery.year)
    val entityPosition = kbpQuery.begOffset
    val uniqueCandidateMap = candidateStrings.groupBy[String](f=> f)
    val candidateDistanceTuples = for(uniqueCandidate <- uniqueCandidateMap.keys) yield {
      var nextIndex = rawDoc.indexOf(uniqueCandidate)
      var minDistance = rawDoc.length()
      while(nextIndex != -1){
        val proximity = entityPosition - nextIndex
        if( proximity > 0){
          minDistance = math.min(minDistance, proximity)
        }
        nextIndex = rawDoc.indexOf(uniqueCandidate,nextIndex+1)
      }
      (uniqueCandidate,minDistance)
    }
    candidateDistanceTuples.toList.sortBy(f => f._2).map(x => x._1)
  }
  
  private def findBestOrganizationString(kbpQuery: KBPQuery, candidateStrings: List[String]) :String = {
    val originalString = kbpQuery.name.trim()
    val sortedCandidateStrings = sortCandidateStringsByProximity(kbpQuery,candidateStrings)
    val rawDoc = SolrHelper.getRawDoc(kbpQuery.doc,kbpQuery.year)


    try{
    val accronymRegex = new Regex("\\([^\\)\\(]{0,15}"+originalString+"[^\\)\\(]{0,15}\\)")
    //if the organization is an acronym
    if(originalString.forall(p => p.isUpper) || accronymRegex.findFirstIn(rawDoc).isDefined ){
            
      for(cs <- sortedCandidateStrings){
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
      
      // if in parentheses and nothing was found...
      //val parenthesisRegexPattern = new Regex("([A-Z]\\w+ (\\w+ )*[A-Z]\\w+)[\\.\\s]*\\([^\\)\\(]{0,5}"+originalString+"[^\\)\\(]{0,5}\\)")
      val accRegexPattern = new Regex("(["+originalString(0).toUpper+originalString(originalString.length()-1).toUpper+"][\\S]+ ([\\S]+ ){0,2}[A-Z][\\S]+).{0,15}"+originalString)
      val accronymMatch = accRegexPattern.findFirstMatchIn(rawDoc)
      if(accronymMatch.isDefined){
        var expandedString = accronymMatch.get.group(1)
        if(stopWords.contains(expandedString.split(" ")(0).toLowerCase())){
          expandedString = expandedString.split(" ").drop(1).mkString(" ")
        }
        return expandedString
      }
      
    }
    }
    catch{
      case e: Exception => {
        
      }
    }
    
    //non caps organization, check if there is a longer string than the original
    //name with the original name as the rightmost word
	var probablyOrganization = true  
    var originalStringIsLocation = false
    val namedEntityCollection = queryNamedEntityCollectionMap.get(kbpQuery.id)
    val locations = namedEntityCollection.locations
	  
    for(loc <- locations){
      if(loc.contains(originalString)){
        originalStringIsLocation = true
      }
    }
	  
    if(originalStringIsLocation){
      probablyOrganization = false
      if(kbpQuery.sportsSense.getOrElse(false)){
        probablyOrganization = true
      }
    }
	
	  
	  
    if(probablyOrganization){
        //do this if original String is not refferring to a location
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
    for(cs <- sortedCandidateStrings){
      if(cs.toLowerCase().startsWith(originalString.toLowerCase()) && cs.length() > originalString.length() && cs.split(" ").length ==1){
        return cs
      }
    }
    
    originalString
    
  }
  
  def locationCasing(str: String) :String ={
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
    val sortedCandidateStrings = sortCandidateStringsByProximity(kbpQuery,candidateStrings)
    var candidates = List[String]()
    val originalWords = originalString.split(" ")
    for(cs <- sortedCandidateStrings){
      val size = cs.split(" ").length
      var index = 0
      while(index < (size-1)){
	      val words = cs.split(" ").drop(index)
	      if( (words.length > (originalWords.length +1)) &&
	          (words.take(originalWords.length).mkString(" ").toLowerCase() == originalString.toLowerCase()) &&
	          (words(originalWords.length) == "," || words(originalWords.length) == "in")){
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
          if(cs != originalString && cs != "United States"){
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
        val origQuote = originalString.replaceAll("\\(|\\)", "")
        val locationRegex = new Regex("("+origQuote+"|"+origQuote.toLowerCase()+"|"+origQuote.toUpperCase()+"),\\s?([A-Z][\\S]+)[\\s\\.\\?!,]")
        val sourceText = SolrHelper.getRawDoc(kbpQuery.doc,kbpQuery.year)
        val candidates = scala.collection.mutable.Map[String,Int]()
        for( locationRegex(containedLoc,containerLoc) <- locationRegex.findAllMatchIn(sourceText); fullLocation = expandAbbreviation(locationCasing(containedLoc+", " +containerLoc)).split(",");
             if locationContainsLocation(fullLocation(1).trim(),fullLocation(0).trim())) {
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
        //sort by distance to original string
        val containerStrings = containerMap.keys
        val sortedContainerStrings = sortCandidateStringsByProximity(kbpQuery,containerStrings.toList)
        locationCasing(originalString + ", " + sortedContainerStrings.head)
//        var largestCount =0
//        var largestContainer = ""
//        for(containerCandidate <- containerMap){
//          val container = containerCandidate._1
//          val count = containerCandidate._2
//          if(count > largestCount){
//            largestCount = count
//            largestContainer = container
//          }
//        }
//        locationCasing(originalString +", " + largestContainer)
      }
    }
    else{
       val candidate = candidates.head
       expandAbbreviation(locationCasing(candidate))
      }
  }
  private def findBestPersonString(kbpQuery: KBPQuery, candidateStrings: List[String], probablyPerson: Boolean) :String = {
      val originalString = kbpQuery.name.trim()
      for(cs <- sortCandidateStringsByProximity(kbpQuery,candidateStrings)){
        val words = cs.split(" ")
        val originalWords = originalString.split(" ")
        if( (words.length > originalWords.length) &&
        		( (words.takeRight(originalWords.length).mkString(" ") == originalString) ||
        		   (words.take(originalWords.length).mkString(" ") == originalString)) &&
        		   (words.length < 4)){
          return (words mkString " ") 
        }
      }

      if(probablyPerson){
      //try a conservative name regex if nothing from Stanford NER was found
	      val nameRegex = """(\.|(\s[a-z]+\s))([A-Z]\w+\s[A-Z]\w+)(\.|(\s[a-z]+\s))""".r
	      val rawDoc = SolrHelper.getRawDoc(kbpQuery.doc,kbpQuery.year)
	      val nameList = for(nameMatch <- nameRegex.findAllMatchIn(rawDoc); name = nameMatch.group(3); if name.contains(originalString)) yield name
	      val sortedNameList = sortCandidateStringsByProximity(kbpQuery,nameList.toList)
	      if(sortedNameList.headOption.isDefined){
	        return sortedNameList.head
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
  
  def haveNamedEntityInCommon(baseDir: String, linkName: String, queryId: String, targetNamedEntities: Option[List[String]] = None): Boolean = {
    val namedEntityCollection = queryNamedEntityCollectionMap.get.get(queryId).get
    val namedEntities = namedEntityCollection.locations ::: namedEntityCollection.organizations ::: namedEntityCollection.people
    val sourceAssociatedNamedEntities = namedEntities.filter(p => !p.toLowerCase().contains(linkName.toLowerCase()))
    val kbpQueryHelper = KBPQuery.getHelper(baseDir, year)
    val wikiMap = kbpQueryHelper.wikiMap
    val kbContextMapFile = kbpQueryHelper.kbContextMapFile
    val kbId = wikiMap.get(linkName).getOrElse("")
      

    var targetAssociatedNamedEntitiesOption :Option[List[String]] = None
    if(targetNamedEntities.isDefined){
      targetAssociatedNamedEntitiesOption = targetNamedEntities
    }
    else{
	    var kbContext = ""
	    breakable{
		    using(io.Source.fromFile(kbContextMapFile, "UTF8")) { source =>
		      val lines = source.getLines
		      val tabSplit = """\t""".r
		      lines.foreach(f => {
		        if(tabSplit.split(f)(0).trim() == kbId){
		          try{
		            kbContext = tabSplit.split(f)(1).trim()
		          }
		          catch{
		            case e: Exception => {
		              kbContext = " "
		            }
		          }
		          break
		        }
		      })
		     }
	    }
	    val context = kbContext
	    if(context == ""){
	      true
	    }
	    else{
          targetAssociatedNamedEntitiesOption = Some(scala.collection.JavaConversions.asScalaIterable(kbpQueryHelper.corefHelper.getNamedEntities(context)).toList)	      
	    }
      }
      println("Query " + queryId)
      println("KB " + kbId)
      println("Sourced Named Entities: ")
      for(ne <- sourceAssociatedNamedEntities){
        println(ne)
      }
      println("KB Named Entities: ")
      for(ne <- targetAssociatedNamedEntitiesOption.get){
        println(ne)
      }
      //sourceAssociatedNamedEntities.exists(p => targetAssociatedNamedEntities.exists(q => (p.split(" ")exists(x => q.contains(x))))) //lenient
      sourceAssociatedNamedEntities.exists(p => targetAssociatedNamedEntitiesOption.get.exists(q => q== p))//strict
    }
}
