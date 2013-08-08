package edu.knowitall.tac2013.entitylinking

import KBPQuery.parseKBPQueries
import edu.knowitall.browser.entity.EntityLinker
import edu.knowitall.browser.entity.batch_match
import edu.knowitall.browser.entity.StringMatchCandidateFinder
import edu.knowitall.browser.entity.CrosswikisCandidateFinder
import edu.knowitall.browser.entity.EntityTyper
import edu.knowitall.common.Resource.using
import edu.knowitall.tac2013.entitylinking.utils.WikiMappingHelper
import scopt.OptionParser
import edu.knowitall.tac2013.entitylinking.utils.FormattedOutputToHumanReadableOutputConverter

object RunKBPEntityLinkerSystem {
  
  var baseDir = "/scratch/"
  
  val clusterCounter = new java.util.concurrent.atomic.AtomicInteger(0)
  val fbidClusterMap = new scala.collection.mutable.HashMap[String, String]
  def nextCluster = "NIL%04d" format clusterCounter.getAndIncrement()
  def fbidCluster(fbid: String) = fbidClusterMap.getOrElseUpdate(fbid, nextCluster)
    
  def linkQueries(queries: Seq[KBPQuery], baseDir :String = "/scratch/"): Seq[FormattedOutput] = {
    
    val linkerSupportPath = new java.io.File(baseDir)
    val linker = new EntityLinker(
    		new batch_match(linkerSupportPath),
    		new CrosswikisCandidateFinder(linkerSupportPath, 0.4, 400),
    		new EntityTyper(linkerSupportPath)
    		)
    
    for(q <- queries) yield {
      println(q.id)
      val link = linker.getBestEntity(q.name,q.corefSourceContext)
      if(link == null){
        new FormattedOutput(q.id,nextCluster,0.0)
      }
      else{
        val nodeId = KBPQuery.wikiMap.getOrElse(throw new Exception("Did not activate KBP Query")).get(link.entity.name)
        new FormattedOutput(q.id,nodeId.getOrElse(fbidCluster(link.entity.fbid)),link.score)
      }
    }
  }
  
  
  
  def main(args: Array[String]) {
 
    val argParser = new OptionParser() {
      arg("baseDir", "Path to base directory with entitylinking files.", { s => baseDir = s })
    }

    if(!argParser.parse(args)) return
    
    KBPQuery.activate(baseDir)
    
    val queries = parseKBPQueries(getClass.getResource("tac_2012_kbp_english_evaluation_entity_linking_queries.xml").getPath()).toSeq
    val answers = linkQueries(queries,baseDir)
    val queryAnswerList = queries zip answers
    val humanReadableOutput = for(qa <- queryAnswerList) yield {
      new FormattedOutputToHumanReadableOutputConverter(qa._2,qa._1)
    }
    val out = new java.io.PrintStream("./sysoutput.txt")
    for(a <- answers){
      out.println(a)
    }
  }
}
