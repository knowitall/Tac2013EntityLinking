package edu.knowitall.tac2013.entitylinking.benchmark

import edu.knowitall.tac2013.entitylinking.utils.FormattedOutputToHumanReadableOutputConverter
import edu.knowitall.tac2013.entitylinking.FormattedOutput
import edu.knowitall.tac2013.entitylinking.KBPQuery
import scopt.OptionParser

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
    
    "%.03f" format entitySums.sum.toDouble / entitySums.size.toDouble
  }
  
  def b3Recall = {
    
    val entitySums = queryIds.map { qid =>
      // get set of entities in the same cluster
      val e1 = goldQueryMap(qid)
      val cluster = goldClusterMap(e1.kbLink)
      val parSum = cluster.map({ e2 => b3Correct(e1.queryId, e2.queryId) }).count(identity)
      parSum.toDouble / cluster.size.toDouble
    }
    
    "%.03f" format entitySums.sum.toDouble / entitySums.size.toDouble
  }
  
  def benchmarkOutput: Seq[String] = {
    
    var numCorrect = 0
    var numNilOk = 0
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
      if (goldAnswerFromKb && sysAnswerFromKb && goldAnswer.kbLink == sysAnswer.kbLink) {
        numCorrect += 1
        kbLinkReport("CORRECT    ", sysAnswer, goldAnswer)
      }
      // if we linked to a different kbId...
      else if (goldAnswerFromKb && sysAnswerFromKb && goldAnswer.kbLink != sysAnswer.kbLink) {
        numWrongKb += 1
        kbLinkReport("WRONG KBID ", sysAnswer, goldAnswer)
      // if we linked to kb, they linked to nil, or vice versa...
      } else if (goldAnswerFromKb && !sysAnswerFromKb) {
        numKbExp += 1
        kbLinkReport("EXP KBID   ", sysAnswer, goldAnswer)
      } else if (!goldAnswerFromKb && sysAnswerFromKb) {
        numNilExp += 1
        kbLinkReport("EXP NIL    ", sysAnswer, goldAnswer)
      // we both linked to nil
      } else {
        numNilOk += 1
        kbLinkReport("NIL OK   ", sysAnswer, goldAnswer)
      }
    }
    
    comparisons ++ Seq("", "SUMMARY",
        s"Number of exact KBID matches:    $numCorrect", 
        s"Number of NILXXX-NILYYY matches: $numNilOk", 
        s"Number of NIL-KB mismatches:     $numKbExp",
        s"Number of KB-NIL mismatches:     $numNilExp",
        s"B^3 Prec:                        $b3Precision",
        s"B^3 Recall:                      $b3Recall")
  }
  
  def kbLinkReport(msg: String, system: FormattedOutput, expected: FormattedOutput): String = {
    val expString = if (msg.contains("KBID")) {
      val pretty = makePretty(expected)
      "\tEXPECTED:\t" + pretty.kbTitle + "\t" + pretty.kbSentence
    } else { "" }
    msg + "\t" + system.kbLink + "\t" + expected.kbLink + "\t" + makePretty(system) + expString
  }
  
  def kbLinkIncorrect(msg: String, system: FormattedOutput, expected: FormattedOutput): String = {
    msg + "\t" + system.kbLink + "\t" + expected.kbLink + "\t" + makePretty(system)
  }
}

object Benchmarker {
  
  import java.io.File
  import edu.knowitall.common.Resource.using
  import scala.io.Source
  import edu.knowitall.tac2013.entitylinking.KBPQuery.parseKBPQueries
  import edu.knowitall.tac2013.entitylinking.RunKBPEntityLinkerSystem
  
  def main(args: Array[String]): Unit = {

    KBPQuery.activate("/scratch")
    
    var baseDir = "/scratch/"
    var systemSort = true
    var benchmarkSort = false
    var querySort = false
      
    val parser = new OptionParser("Benchmarker") {
      opt("basedir", "basedir", { s => baseDir = s })
      opt("systemSort", "Sort queries by system cluster id. (default)", { systemSort = true})
      opt("benchmarkSort", "Sort queries by benchmark set cluster id.", { benchmarkSort = true})
      opt("querySort", "Sort queries by id.", { querySort = true})
    }
    
    if (!parser.parse(args)) return
    
    KBPQuery.activate(baseDir)
    
    val queries = parseKBPQueries(getClass.getResource("/edu/knowitall/tac2013/entitylinking/tac_2012_kbp_english_evaluation_entity_linking_queries.xml").getPath())
    val answerUrl = getClass.getResource("tac_2012_kbp_english_evaluation_entity_linking_query_types.tab")
    val answers = using(Source.fromURL(answerUrl, "UTF8")) { answerSrc => answerSrc.getLines.map(FormattedOutput.readFormattedOutput).toList }
    val results = RunKBPEntityLinkerSystem.linkQueries(queries)
    
    val sortType = if (querySort) QueryIdSort else if (benchmarkSort) BenchmarkClusterSort else SystemClusterSort
    
    new Benchmarker(sortType, queries, results, answers).benchmarkOutput foreach println
    
  }
}