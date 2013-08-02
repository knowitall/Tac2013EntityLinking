package edu.knowitall.tac2013.entitylinking

import KBPQuery.parseKBPQueries
import edu.knowitall.browser.entity.EntityLinker
import edu.knowitall.browser.entity.batch_match
import edu.knowitall.browser.entity.StringMatchCandidateFinder
import edu.knowitall.browser.entity.EntityTyper

object RunKBPEntityLinkerSystem {
  
  
  private def linkQueries(queries: List[KBPQuery]): Unit = {
    
    val linkerSupportPath = new java.io.File("/scratch/resources/entitylinkingResources/")
    val linker = new EntityLinker(
    		new batch_match(linkerSupportPath),
    		new StringMatchCandidateFinder(linkerSupportPath),
    		new EntityTyper(linkerSupportPath)
    		)
    
    
    for(q <- queries){
      val link = linker.getBestEntity(q.name, List(q.sourceSentence))
      
    }
    
    
  }
  
  
  
  def main(args: Array[String]) {
    
    
    val queries = parseKBPQueries()
    linkQueries(queries)
    
    
  }
  

}