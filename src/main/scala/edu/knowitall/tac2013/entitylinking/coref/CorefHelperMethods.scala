package edu.knowitall.tac2013.entitylinking.coref
import edu.knowitall.common.Resource.using
import edu.knowitall.tac2013.entitylinking.KBPQuery
import edu.knowitall.browser.entity.EntityLinker
import edu.knowitall.tac2013.entitylinking.SolrHelper

object CorefHelperMethods {
  val queryMentionMap = {
    System.err.println("Loading query to Coref String Mentions map...")
    val corefFile = getClass.getResource("/edu/knowitall/tac2013/entitylinking/coref/corefStringMentions.txt").getPath()
    using{scala.io.Source.fromFile(corefFile)}{ source =>
      source.getLines.map{ line =>
        line.split("\t") match{
        	case Array(qId, e  @ _*) => {(qId,for(mention <- e) yield mention)}
        	case _ => throw new RuntimeException("Error parsing coref mentions")
        }
      } toMap
    }
  }
  
  private val queryNamedEntityCollectionMap = {
    System.err.println("Loading query to Named Entities map...")
    val namedEntityFile = getClass.getResource("/edu/knowitall/tac2013/entitylinking/coref/namedEntities.txt").getPath()
    using{scala.io.Source.fromFile(namedEntityFile)}{ 
      source => {
        val lines = source.getLines.toList
        
        val queryNamedEntityCollections = lines.sliding(4)
        
        queryNamedEntityCollections.map{ necLines => {
          val firstLine = necLines(0)
          val firstLineValues = firstLine.split("\t")
          val qId = firstLineValues(0)
          var qType = ""
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
    }
  }
  
  private class NamedEntityCollection(val qId:String, val qType:String,
      val matchingNamedEntities: List[String],
      val organizations: List[String],
      val locations: List[String],
      val people: List[String]){
  }
  
  
  def identifyBestEntityStringByLinkerScore(q: KBPQuery, linker: EntityLinker): String = {
     var bestCandidate = q.name
     var bestScore = 0.0
     val candidates = q.name :: queryMentionMap(q.id).toList
     val uniqueCandidates = candidates.toSet.toList
     for(uc <- uniqueCandidates){
          print(uc + "\t")
          val link = linker.getBestEntity(uc,q.corefSourceContext)
          if(link != null){
          println(link.score)
	          if(link.score > bestScore){
	            bestCandidate =uc
	            bestScore = link.score
	          }
          }
          else{
            println("null")
          }
     }
     bestCandidate
  }
  
  def identifyBestEntityStringByRules(q: KBPQuery): String = {
    var alternateName = q.name
    val namedEntityCollection = queryNamedEntityCollectionMap.get(q.id).get
    val entityType = namedEntityCollection.qType
    if(entityType != ""){
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
	          return words mkString " "
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
    
    originalString
    
  }
  private def findBestLocationString(originalString: String, candidateStrings: List[String]) :String = {
    val originalWords = originalString.split(" ")
    for(cs <- candidateStrings){
      val words = cs.split(" ")
      if( (words.length > (originalWords.length +1)) &&
          (words.take(originalWords.length).mkString(" ").toLowerCase() == originalString.toLowerCase()) &&
          (words(originalWords.length) == ",")){
        return words.take(originalWords.length).mkString(" ") + ", " + words.drop(originalWords.length+1).mkString(" ")
      }
    }
    originalString
  }
  private def findBestPersonString(originalString: String, candidateStrings: List[String]) :String = {
      for(cs <- candidateStrings){
        val words = cs.split(" ")
        val originalWords = originalString.split(" ")
        if( (words.length > originalWords.length) &&
        		( (words.takeRight(originalWords.length).mkString(" ") == originalString) ||
        		   (words.take(originalWords.length).mkString(" ") == originalString) )){
          return words mkString " "
        }
      }
      originalString
  }


}