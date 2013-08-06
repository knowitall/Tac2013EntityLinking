package edu.knowitall.tac2013.entitylinking.benchmark

import edu.knowitall.tac2013.entitylinking.utils.FormattedOutputToHumanReadableOutputConverter
import edu.knowitall.tac2013.entitylinking.FormattedOutput
import edu.knowitall.tac2013.entitylinking.KBPQuery
import scopt.OptionParser

class Benchmarker(val queries: Seq[KBPQuery], val systemResults: Seq[FormattedOutput], val goldSet: Seq[FormattedOutput]) {

  val queryIds   = queries.map(q => q.id).sorted
  def queryIdPairs = queryIds.flatMap(qid => queryIds.map(qid2 => (qid, qid2))).filter(p => p._1 != p._2)
  val queryMap   = queries.map(q => (q.id, q)).toMap
  val resultsMap = systemResults.map(f => (f.queryId, f)).toMap
  val goldMap    = goldSet.map(f => (f.queryId, f)).toMap
  
  def makePretty(f: FormattedOutput) = new FormattedOutputToHumanReadableOutputConverter(f, queryMap(f.queryId))
  
  // implements the definition of correctness defined in the EL task desc.
  def b3Correct(e1: String, e2: String): Boolean = {
    // in same set in system output?
    val sysE1 = resultsMap(e1)
    val sysE2 = resultsMap(e2)
    val goldE1 = goldMap(e1)
    val goldE2 = goldMap(e2)
    val sysSameSet = sysE1.kbLink == sysE2.kbLink
    val goldSameSet= goldE1.kbLink == goldE2.kbLink
    val sysKb  = sysE1.kbLink.startsWith("E")
    val goldKb = goldE1.kbLink.startsWith("E")
    val sameLinkTypes = if (sysKb && goldKb) sysE1.kbLink == goldE1.kbLink else sysKb == goldKb
    sysSameSet && goldSameSet && sameLinkTypes
  }
  
  def benchmarkOutput: Seq[String] = {
    
    var numCorrect = 0
    var numNilOk = 0
    var numMismatch = 0
    
    val comparisons = for (qid <- queryIds) yield {
      val goldAnswer = goldMap(qid)
      val sysAnswer = resultsMap(qid)
      val goldAnswerFromKb = goldAnswer.kbLink.startsWith("E")
      val sysAnswerFromKb = sysAnswer.kbLink.startsWith("E")
      if (goldAnswerFromKb && sysAnswerFromKb && goldAnswer.kbLink != sysAnswer.kbLink) {
        numCorrect += 1
        "CORRECT  " + sysAnswer.kbLink + ":\t" + makePretty(sysAnswer)
      }
      // if we linked to a different kbId...
      else if (goldAnswerFromKb && sysAnswerFromKb && goldAnswer.kbLink != sysAnswer.kbLink) {
        numMismatch += 1
        kbLinkMismatch(sysAnswer, goldAnswer)
      // if we linked to kb, they linked to nil, or vice versa...
      } else if (goldAnswerFromKb ^ sysAnswerFromKb) {
        numMismatch += 1
        kbLinkMismatch(sysAnswer, goldAnswer)
      // we both linked to nil
      } else {
        numNilOk += 1
        "NIL OK   " + sysAnswer.kbLink + ":\t" + makePretty(sysAnswer)
      }
    }
    
    comparisons ++ Seq("", s"Num CORRECT: $numCorrect", s"Num NIL OK: $numNilOk", s"num Mismatch: $numMismatch")
  }
  
  def kbLinkMismatch(system: FormattedOutput, expected: FormattedOutput): String = {
    val sys = "FOUND    " + system.kbLink + ":\t" + makePretty(system)
    val exp = "EXPECTED " + expected.kbLink + ":\t" + makePretty(expected)
    sys + "\n" + exp
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
      
    val parser = new OptionParser("Benchmarker") {
      opt("basedir", "basedir", { s => baseDir = s })
    }
    
    if (!parser.parse(args)) return
    
    KBPQuery.activate(baseDir)
    
    val queries = parseKBPQueries(getClass.getResource("/edu/knowitall/tac2013/entitylinking/tac_2012_kbp_english_evaluation_entity_linking_queries.xml").getPath())
    val answerUrl = getClass.getResource("tac_2012_kbp_english_evaluation_entity_linking_query_types.tab")
    val answers = using(Source.fromURL(answerUrl, "UTF8")) { answerSrc => answerSrc.getLines.map(FormattedOutput.readFormattedOutput).toList }
    val results = RunKBPEntityLinkerSystem.linkQueries(queries)
    
    new Benchmarker(queries, results, answers).benchmarkOutput foreach println
    
  }
}