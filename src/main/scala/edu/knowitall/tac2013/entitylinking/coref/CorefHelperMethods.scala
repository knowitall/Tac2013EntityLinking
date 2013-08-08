package edu.knowitall.tac2013.entitylinking.coref
import edu.knowitall.common.Resource.using
import edu.knowitall.tac2013.entitylinking.KBPQuery
import edu.knowitall.browser.entity.EntityLinker
import edu.knowitall.tac2013.entitylinking.SolrHelper

object CorefHelperMethods {
  val queryMentionMap = {
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
    val queryTypeData = scala.collection.JavaConversions.asScalaIterable(KBPQuery.corefHelper.getCorefTypes(SolrHelper.getRawDoc(q.doc), q.begOffset)).toList
    if(queryTypeData.isEmpty){
      q.name
    }
    else{
      val entityType = queryTypeData(0)
      entityType match{
        case "ORGANIZATION" => { findBestOrganizationString(q.name.trim(),queryTypeData.drop(1))}
        case "LOCATION" => {findBestLocationString(q.name.trim(),queryTypeData.drop(1))}
        case "PERSON" => {findBestPersonString(q.name.trim(),queryTypeData.drop(1))}
      }
    }
  }
  
  private def findBestOrganizationString(originalString: String, candidateStrings: List[String]) :String = {
    
    //if the organization is an acronym
    if(originalString.forall(p => p.isUpper)){
      
      for(cs <- candidateStrings){
        val words = cs.split(" ").takeRight(originalString.length())
        var goodCandidate = true
        var index = 0
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
    
    //non caps organization, check if there is a longer string than the original
    //name with the original name as the rightmost word
    else{
      for(cs <- candidateStrings){
        val words = cs.split(" ")
        val originalWords = originalString.split(" ")
        if( (words.length > originalWords.length) &&
            (words.takeRight(originalWords.length).mkString(" ") == originalString)){
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
          (words.take(originalWords.length).mkString(" ") == originalString) &&
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