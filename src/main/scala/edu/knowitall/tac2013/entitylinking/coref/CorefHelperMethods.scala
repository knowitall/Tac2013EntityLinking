package edu.knowitall.tac2013.entitylinking.coref
import edu.knowitall.common.Resource.using
import edu.knowitall.tac2013.entitylinking.KBPQuery
import edu.knowitall.browser.entity.EntityLinker

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
  
  
  def identifyBestEntityString(q: KBPQuery, linker: EntityLinker): String = {
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

}