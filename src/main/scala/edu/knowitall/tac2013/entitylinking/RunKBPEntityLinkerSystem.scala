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
import edu.knowitall.tac2013.entitylinking.coref.CorefHelperMethods.identifyBestEntityStringByRules


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
      val entityString = identifyBestEntityStringByRules(q)
      q.entityString = entityString
      val link = linker.getBestEntity(entityString,q.corefSourceContext)
      println(q.id + "\t" + q.name +"\t" + entityString)
      if(link == null){
        //if link is null and there is a better entity string
        //than the one given in KBP check KB for
        var answer : Option[FormattedOutput] = None
        if(q.entityString != q.name){
          val kbIdOption = KBPQuery.kbTitleToIdMap.get.get(q.entityString)
          if(kbIdOption.isDefined){
            answer = Some(new FormattedOutput(q.id,kbIdOption.get,.9))
          }
        }
        if(answer.isDefined){
          answer.get
        }
        else{
         new FormattedOutput(q.id,nextCluster,0.0)
        }
      }
      else{
        val nodeId = KBPQuery.wikiMap.getOrElse(throw new Exception("Did not activate KBP Query")).get(link.entity.name)
        new FormattedOutput(q.id,nodeId.getOrElse(fbidCluster(link.entity.fbid)),link.combinedScore)
      }
    }
  }
  
  
  
  def main(args: Array[String]) {
 
    var outputStream = System.out
    var humanReadable = false
    
    val argParser = new OptionParser() {
      arg("baseDir", "Path to base directory with entitylinking files.", { s => baseDir = s })
      opt("outputFile", "Path to output file, default stdout", { s => outputStream = new java.io.PrintStream(s, "UTF8") })
      opt("humanReadable", "Produce detailed human readable output (instead of submission format)", { humanReadable =  true})
    }

    if(!argParser.parse(args)) return
    
    KBPQuery.activate(baseDir)
    
    val queries = parseKBPQueries(getClass.getResource("tac_2012_kbp_english_evaluation_entity_linking_queries.xml").getPath()).toSeq
    val answers = linkQueries(queries)
    val answerStrings = if (humanReadable) {
      val queryAnswerList = queries zip answers
      for (qa <- queryAnswerList) yield {
        new FormattedOutputToHumanReadableOutputConverter(qa._2, qa._1).toString
      }
    } else { 
      answers.map(_.toString)
    }

    for (a <- answerStrings) {
      outputStream.println(a)
    }
  }
}
