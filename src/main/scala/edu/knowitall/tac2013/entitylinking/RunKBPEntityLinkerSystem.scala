package edu.knowitall.tac2013.entitylinking

import edu.knowitall.browser.entity.EntityLinker
import edu.knowitall.browser.entity.batch_match
import edu.knowitall.browser.entity.StringMatchCandidateFinder
import edu.knowitall.browser.entity.CrosswikisCandidateFinder
import edu.knowitall.browser.entity.EntityTyper
import edu.knowitall.common.Resource.using
import edu.knowitall.tac2013.entitylinking.utils.WikiMappingHelper
import scopt.OptionParser
import edu.knowitall.tac2013.entitylinking.coref.CorefHelperMethods
import edu.knowitall.tac2013.entitylinking.utils.FormattedOutputToHumanReadableOutputConverter
import edu.knowitall.tac2013.entitylinking.classifier.LinkClassifier
import edu.knowitall.tac2013.entitylinking.classifier.MentionPairClassifier
import edu.knowitall.tac2013.entitylinking.classifier.Mention
import edu.knowitall.tac2013.entitylinking.classifier.MentionPair
import edu.knowitall.tac2013.entitylinking.utils.ResourceHelper
import edu.knowitall.tac2013.entitylinking.classifier.SportsSenseLabeller
import edu.knowitall.tac2013.entitylinking.utils.SportsHelperMethods

case class RunKBPEntityLinkerSystem(val baseDir: String, val year: String) {

  //var baseDir = "/scratch/resources/entitylinkingResources"

  
  val clusterCounter = new java.util.concurrent.atomic.AtomicInteger(0)
  val fbidClusterMap = new scala.collection.mutable.HashMap[String, String]
  def nextCluster = "NIL%04d" format clusterCounter.getAndIncrement()
  def fbidCluster(fbid: String) = fbidClusterMap.getOrElseUpdate(fbid, nextCluster)

  val linkClassifier = new LinkClassifier(baseDir)

  val linkerSupportPath = new java.io.File(baseDir)
  val linker = new EntityLinker(
    		new batch_match(linkerSupportPath),
    		new CrosswikisCandidateFinder(linkerSupportPath, 0.00, 1),
    		new EntityTyper(linkerSupportPath)
  		)
  
  def linkQueries(queries: Seq[KBPQuery], year: String, sportsClassifyOn:Boolean): Seq[FormattedOutput] = {

    for(q <- queries) yield linkQuery(q, linker, linkClassifier, sportsClassifyOn)
  }

  def linkQuery(q: KBPQuery, linker: EntityLinker, linkClassifier: LinkClassifier, sportsClassifyOn: Boolean): FormattedOutput = {
    if(sportsClassifyOn){
      q.sportsSense = SportsSenseLabeller.labelSportsSense(q)
    }
    val entityString = CorefHelperMethods.get(year).identifyBestEntityStringByRules(q)
    q.entityString = entityString
    println(q.id + "\t" + q.name + "\t" + entityString)
    val linkOpt = linker.getBestEntity(entityString, q.corefSourceContext)
    q.highestLinkClassifierScore = linkOpt match{
      case None => 0.0
      case Some(x) => linkClassifier.score(x)
    }
    val answer = linkOpt.filter(l => linkClassifier.score(l) > 0.84) match {
      case None => {
        //if link is null and there is a better entity string
        //than the one given in KBP check KB for
        var answer: Option[FormattedOutput] = None
        if (q.entityString != q.name) {
          val kbIdOption = KBPQuery.getHelper(baseDir, year).kbTitleToIdMap.get(q.entityString)
          if (kbIdOption.isDefined) {
            answer = Some(new FormattedOutput(q.id, kbIdOption.get, .9))
          }
        }
        if (answer.isDefined) {
          answer.get
        } else {
          new FormattedOutput(q.id, nextCluster, 0.0)
        }
      }
      case Some(link) => {

        var nodeId :Option[String] = None
        nodeId = KBPQuery.getHelper(baseDir, year).wikiMap.get(link.entity.name)

        new FormattedOutput(q.id, nodeId.getOrElse(nextCluster), link.combinedScore)
      }
    }
    
    if(q.sportsSense.isDefined){
      if(q.sportsSense.get == true){
        if(SportsHelperMethods(baseDir, year).isLocation(answer.kbLink)){
          return new FormattedOutput(q.id,nextCluster,.5)
        }
      }
    }
    answer
  }

  def clusterNils(answers: Seq[FormattedOutput], queries: Seq[KBPQuery]) :Seq[FormattedOutput] = {
    val queryMap = queries.map(f => (f.id,f))toMap
    val nilClusters = answers.filter(p => p.kbLink.startsWith("NIL")).filter(
        q => {
          val kbpQuery = queryMap.get(q.queryId).get
          val entityString = kbpQuery.entityString
          val name = kbpQuery.name
          (entityString != name)
        }).groupBy(f => queryMap.get(f.queryId).get.entityString).map(f => {
          (f._1,f._2.head.kbLink)
        })toMap
        
    var newAnswerSeq = Seq[FormattedOutput]()
    for(answer <- answers){
      if(!answer.kbLink.startsWith("NIL")){
        newAnswerSeq = newAnswerSeq :+ answer
      }
      else if(!nilClusters.contains(queryMap.get(answer.queryId).get.entityString)){
        newAnswerSeq = newAnswerSeq :+ answer
      }
      else{
        val nilClusterId = nilClusters.get(queryMap.get(answer.queryId).get.entityString).get
        if(answer.kbLink != nilClusterId){
          println("Changing from " + answer.kbLink + " to " + nilClusterId )
        }

        val newAnswer =  new FormattedOutput(answer.queryId,nilClusterId,.5)
        newAnswerSeq = newAnswerSeq :+ newAnswer
      }
    }
    
    newAnswerSeq.toList
  }
}

object RunKBPEntityLinkerSystem {
  

  def main(args: Array[String]) {
 
    var outputStream = System.out
    var humanReadable = false
    var year = ""
    var sportsClassifyOn = false
    var baseDir = "/scratch/"
      
    val argParser = new OptionParser() {
      arg("baseDir", "Path to base directory with entitylinking files.", { s => baseDir = s })
      arg("year", "Year of queries to run on", {s => year =s })
      opt("outputFile", "Path to output file, default stdout", { s => outputStream = new java.io.PrintStream(s, "UTF8") })
      opt("humanReadable", "Produce detailed human readable output (instead of submission format)", { humanReadable =  true})
      opt("sportsClassify","Turn on sports classification", {sportsClassifyOn = true})
    }

    if(!argParser.parse(args)) return
    
    if(year != "2010" &&
        year != "2011" &&
        year != "2012" &&
        year != "2013"){
      throw new Exception("Year must be 2010,2011,2012,or 2013")
    }
    
    val system = RunKBPEntityLinkerSystem(baseDir, year)
    
    ResourceHelper.initialize(baseDir, year)
    val kbpQueryHelper = KBPQuery.getHelper(baseDir,year)

    val queries = kbpQueryHelper.parseKBPQueries(getClass.getResource("tac_"+year+"_kbp_english_evaluation_entity_linking_queries.xml").getPath()).toSeq
    val answers = system.clusterNils(system.linkQueries(queries,year,sportsClassifyOn),queries)

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
