package edu.knowitall.tac2013.entitylinking.benchmark

import edu.knowitall.tac2013.entitylinking.utils.FormattedOutputToHumanReadableOutputConverter
import edu.knowitall.tac2013.entitylinking.FormattedOutput
import edu.knowitall.tac2013.entitylinking.KBPQuery
import scopt.OptionParser
import java.io.PrintWriter
import edu.knowitall.tac2013.entitylinking.utils.ResourceHelper
import edu.knowitall.tac2013.entitylinking.Clusterer

sealed trait SortType
case object SystemClusterSort extends SortType
case object BenchmarkClusterSort extends SortType
case object QueryIdSort extends SortType

class Benchmarker(val sortType: SortType, val queries: Seq[KBPQuery], val systemResults: Seq[FormattedOutput], val goldSet: Seq[FormattedOutput]) {

  val systemClusterIds = systemResults.map(_.kbLink).distinct.sorted
  val goldClusterIds = goldSet.map(_.kbLink).distinct.sorted
  val resultsQueryMap = systemResults.map(f => (f.queryId, f)).toMap
  val resultsClusterMap = systemResults.groupBy(f => f.kbLink)
  val goldClusterMap = goldSet.groupBy(_.kbLink)
  val systemSortedQueryIds = systemClusterIds.flatMap(cid => resultsClusterMap(cid).map(_.queryId).distinct.sorted)
  val goldSortedQueryIds = goldClusterIds.flatMap(cid => goldClusterMap(cid).map(_.queryId).distinct.sorted)
  val queryIds   = queries.map(q => q.id).sorted
  val queryMap   = queries.map(q => (q.id, q)).toMap
  
  val goldQueryMap    = goldSet.map(f => (f.queryId, f)).toMap
  
  def numOverClustered = resultsClusterMap.count { case (id, outs) =>
    val queries = outs.map(_.queryId).distinct
    val goldIds = queries.map(goldQueryMap.apply).map(_.kbLink).distinct
    goldIds.size > 1
  }
  
  def makePretty(f: FormattedOutput) = new FormattedOutputToHumanReadableOutputConverter(f, queryMap(f.queryId))
  
  // implements the definition of correctness defined in the EL task desc.
  def b3Correct(e1: String, e2: String): Boolean = {
    // in same set in system output?
    val sysE1 = resultsQueryMap(e1)
    val sysE2 = resultsQueryMap(e2)
    val goldE1 = goldQueryMap(e1)
    val goldE2 = goldQueryMap(e2)
    val sysSameSet = sysE1.kbLink == sysE2.kbLink
    val goldSameSet= goldE1.kbLink == goldE2.kbLink
    val sysKb  = sysE1.kbLink.startsWith("E")
    val goldKb = goldE1.kbLink.startsWith("E")
    val sameLinkTypes = if (sysKb && goldKb) sysE1.kbLink == goldE1.kbLink else sysKb == goldKb
    sysSameSet && goldSameSet && sameLinkTypes
  }
  
  def b3Precision = {
    
    val entitySums = queryIds.map { qid =>
      // get set of entities in the same cluster
      val e1 = resultsQueryMap(qid)
      val cluster = resultsClusterMap(e1.kbLink)
      val parSum = cluster.map({ e2 => b3Correct(e1.queryId, e2.queryId) }).count(identity)
      parSum.toDouble / cluster.size.toDouble
    }
    
    entitySums.sum.toDouble / entitySums.size.toDouble
  }
  
  def b3Recall = {
    
    val entitySums = queryIds.map { qid =>
      // get set of entities in the same cluster
      val e1 = goldQueryMap(qid)
      val cluster = goldClusterMap(e1.kbLink)
      val parSum = cluster.map({ e2 => b3Correct(e1.queryId, e2.queryId) }).count(identity)
      parSum.toDouble / cluster.size.toDouble
    }
    
    entitySums.sum.toDouble / entitySums.size.toDouble
  }
  
  def benchmarkOutput: Seq[String] = {
    
    var numCorrect = 0
    var numNilOk = 0
    var numWrongNil = 0
    var numKbExp = 0
    var numNilExp = 0
    var numWrongKb = 0
    
    val sortedQueryIds = sortType match {
      case SystemClusterSort => systemSortedQueryIds
      case BenchmarkClusterSort => goldSortedQueryIds
      case QueryIdSort => queryIds
    }
    
    val comparisons = for (qid <- sortedQueryIds) yield {
      val goldAnswer = goldQueryMap(qid)
      val sysAnswer = resultsQueryMap(qid)
      val goldAnswerFromKb = goldAnswer.kbLink.startsWith("E")
      val sysAnswerFromKb = sysAnswer.kbLink.startsWith("E")
      
      val sysKbid = sysAnswer.kbLink
      val sysCluster = resultsClusterMap.getOrElse(sysKbid, Nil).map(_.queryId).toSet
      val goldKbid = goldAnswer.kbLink
      val theirCluster = goldClusterMap.getOrElse(goldKbid,Nil).map(_.queryId).toSet
      
      if (goldAnswerFromKb && sysAnswerFromKb && goldAnswer.kbLink == sysAnswer.kbLink) {
        numCorrect += 1
        kbLinkReport("CORRECT     ", sysAnswer, goldAnswer)
      }
      // if we linked to a different kbId...
      else if (goldAnswerFromKb && sysAnswerFromKb && goldAnswer.kbLink != sysAnswer.kbLink) {
        numWrongKb += 1
        kbLinkReport("WRONG KB ID ", sysAnswer, goldAnswer)
      // if we linked to kb, they linked to nil, or vice versa...
      } else if (goldAnswerFromKb && !sysAnswerFromKb) {
        numKbExp += 1
        kbLinkReport("EXP KB ID   ", sysAnswer, goldAnswer)
      } else if (!goldAnswerFromKb && sysAnswerFromKb) {
        numNilExp += 1
        kbLinkReport("EXP NIL     ", sysAnswer, goldAnswer)
      // we both linked to nil
      } else if (!sysCluster.equals(theirCluster)) {
        numWrongNil += 1
        kbLinkReport("WRONG NIL ID", sysAnswer, goldAnswer)
      } else {
        numNilOk += 1
        kbLinkReport("NIL OK      ", sysAnswer, goldAnswer)
      }
    }
    
    val prec = b3Precision
    val rec  = b3Recall
    val f1 = (2*prec*rec)/(prec + rec)
    val precStr = "%.03f" format prec
    val recStr  = "%.03f" format rec
    val f1Str   = "%.03f" format f1
    
    comparisons ++ List("", "SUMMARY",
        s"Number of exact KB ID matches:    \t$numCorrect", 
        s"Number of NILXXX-NILYYY matches:  \t$numNilOk", 
        s"Number of Wrong KB ID mismatches: \t$numWrongKb",
        s"Number of Unmerged NILs:          \t$numWrongNil",
        s"Number of NIL-KB mismatches:      \t$numKbExp",
        s"Number of KB-NIL mismatches:      \t$numNilExp",
        s"B^3 Prec:                         \t$precStr",
        s"B^3 Recall:                       \t$recStr",
        s"B^3 F1:                           \t$f1Str",
        "",
        s"Num Overclustered:                \t$numOverClustered")
  }
  
  
  
  def kbLinkReport(msg: String, system: FormattedOutput, expected: FormattedOutput): String = {
    val expString = if (msg.contains("KB ID")) {
      val pretty = makePretty(expected)
      "\tEXPECTED:\t" + pretty.kbTitle + "\t" + pretty.kbSentence
    } else { "" }
    val query = queryMap.get(system.queryId).get
    val sportsClassificationString = query.sportsSense match{
      case None => "Did not run classifier"
      case Some(x) => x match{
        case true => {"Has Sports Sense"}
        case false => {"Does not Have Sports Sense"}
      }
    }
    (msg + "\t" + system.kbLink + "\t" + expected.kbLink + "\t" + makePretty(system) + expString
    + "\t" + "Highest Link Classifier Score:" + "\t" + query.highestLinkClassifierScore + "\t" + "Sports Classification: " + "\t" + sportsClassificationString)
  }
  
  def kbLinkIncorrect(msg: String, system: FormattedOutput, expected: FormattedOutput): String = {
    msg + "\t" + system.kbLink + "\t" + expected.kbLink + "\t" + makePretty(system)
  }
}

object Benchmarker {
  
  import java.io.File
  import edu.knowitall.common.Resource.using
  import scala.io.Source
  import edu.knowitall.tac2013.entitylinking.RunKBPEntityLinkerSystem
  
  def main(args: Array[String]): Unit = {
    
    var baseDir = "/scratch/"
    var systemSort = true
    var benchmarkSort = false
    var querySort = false
    var outputFile = ""
    var year = ""
    var sportsClassifyOn = false
    var fromScratch = false
      
    val parser = new OptionParser("Benchmarker") {
      arg("year", "Year of queries to run on", {s => year =s })
      opt("basedir", "basedir", { s => baseDir = s })
      opt("systemSort", "Sort queries by system cluster id. (default)", { systemSort = true})
      opt("benchmarkSort", "Sort queries by benchmark set cluster id.", { benchmarkSort = true})
      opt("querySort", "Sort queries by id.", { querySort = true})
      opt("outputFile", "output to file", {s => outputFile =s})
      opt("sportsClassify","Turn on sports classification", {sportsClassifyOn = true})
      opt("fromScratch", "Run system from scratch", {fromScratch = true})
    }
    
    if (!parser.parse(args)) return
    
    if(year != "2010" &&
        year != "2011" &&
        year != "2012" &&
        year != "2013"){
      throw new Exception("Year must be 2010,2011,2012,or 2013")
    }
    ResourceHelper.initialize(baseDir, year)

    val system = RunKBPEntityLinkerSystem(baseDir, year)
    
    val kbpQueryHelper = KBPQuery.getHelper(baseDir,year,fromScratch)
    
    val queries = kbpQueryHelper.parseKBPQueries(getClass.getResource("/edu/knowitall/tac2013/entitylinking/tac_"+year+"_kbp_english_evaluation_entity_linking_queries.xml").getPath())
    val answerUrl = getClass.getResource("tac_"+year+"_kbp_english_evaluation_entity_linking_query_types.tab")
    val answers = using(Source.fromURL(answerUrl, "UTF8")) { answerSrc => answerSrc.getLines.map(FormattedOutput.readFormattedOutput).toList }
    //val results = system.clusterNils(system.linkQueries(queries,year,sportsClassifyOn),queries)
    val results = system.clusterNils(system.linkQueries(queries,year,sportsClassifyOn),queries)
    
    val sortType = if (querySort) QueryIdSort else if (benchmarkSort) BenchmarkClusterSort else SystemClusterSort
    
    if(outputFile == ""){
      new Benchmarker(sortType, queries, results, answers).benchmarkOutput foreach println
    }
    else {
      val pw = new PrintWriter(new File(outputFile))
      new Benchmarker(sortType, queries, results, answers).benchmarkOutput.foreach{p => { pw.write(p+"\n") } }
      pw.close()
    }
  }
}