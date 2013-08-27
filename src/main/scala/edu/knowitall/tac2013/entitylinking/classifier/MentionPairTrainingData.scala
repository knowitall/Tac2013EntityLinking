package edu.knowitall.tac2013.entitylinking.classifier

import edu.knowitall.tac2013.entitylinking.KBPQuery
import edu.knowitall.browser.entity.EntityLinker
import edu.knowitall.browser.entity.batch_match
import edu.knowitall.browser.entity.StringMatchCandidateFinder
import edu.knowitall.browser.entity.CrosswikisCandidateFinder
import edu.knowitall.browser.entity.EntityTyper
import edu.knowitall.common.Resource.using
import edu.knowitall.tac2013.entitylinking.utils.WikiMappingHelper
import scopt.OptionParser
import edu.knowitall.tac2013.entitylinking.FormattedOutput
import edu.knowitall.tac2013.entitylinking.utils.FormattedOutputToHumanReadableOutputConverter
import edu.knowitall.tac2013.entitylinking.RunKBPEntityLinkerSystem
import edu.knowitall.tac2013.entitylinking.utils.FormattedOutputToHumanReadableOutputConverter
import edu.knowitall.tool.conf.Labelled

class MentionPairTrainingData(val basePath: String, val year: String) extends Iterable[Labelled[MentionPair]] {
    
  val kbpQueryHelper = KBPQuery.getHelper(basePath, year)
  
  val answerUrl = getClass.getResource("/edu/knowitall/tac2013/entitylinking/benchmark/tac_"+year+"_kbp_english_evaluation_entity_linking_query_types.tab")
  val answers = using(io.Source.fromURL(answerUrl, "UTF8")) { answerSrc => answerSrc.getLines.map(FormattedOutput.readFormattedOutput).toList }
  val answersMap = answers.map(a => (a.queryId, a)).toMap

  private val queries = kbpQueryHelper.parseKBPQueries(getClass.getResource("/edu/knowitall/tac2013/entitylinking/tac_"+year+"_kbp_english_evaluation_entity_linking_queries.xml").getPath()).toSeq
  private val queryAnswers = queries.map(q => (q, answersMap(q.id)))

  private val mentions = queryAnswers.map { case (query, answer) =>
    val humanReadable = new FormattedOutputToHumanReadableOutputConverter(answer, query)
    Mention.from(query, humanReadable)
  }
  
  private val allMentionPairs = mentions.zipWithIndex.flatMap { case (m1, index) =>
    mentions.drop(index + 1).map { m2 => 
      val label = m1.linkId == m2.linkId
      Labelled(label, new MentionPair(m1, m2)) 
    }  
  }

  private lazy val filteredMentionPairs = allMentionPairs.filter { mp =>
    MentionPairTrainingData.mentionPairFilter(mp.item)
  }
  
  private val rand = new scala.util.Random(0)
  
  private lazy val positiveFilteredPairs = filteredMentionPairs.filter(_.label)
  private lazy val negativeFilteredPairs = rand.shuffle(filteredMentionPairs.filterNot(_.label))
  private lazy val positivePairs = rand.shuffle(allMentionPairs.filter(_.label))
  private lazy val negativePairs = rand.shuffle(allMentionPairs.filterNot(_.label))
  
  
  def iterator = rand.shuffle(positivePairs ++ negativePairs.take(positivePairs.size * 10)).iterator.take(3000)
}

object MentionPairTrainingData {
  def mentionPairFilter(mp: MentionPair): Boolean = {
    val m1 = mp.m1
    val m2 = mp.m2
    def m1Context = (m1.corefVec.keySet ++ m1.wideVec.keySet)
    def m2Context = (m2.corefVec.keySet ++ m2.wideVec.keySet)
    val m1String = m1.entityString.toLowerCase
    val m2String = m2.entityString.toLowerCase
    val m1Alt = m1.entityStringUsed.toLowerCase
    val m2Alt = m2.entityStringUsed.toLowerCase
    def m1ContainsM2 = m1Context.exists(c => c.contains(m2String) || c.contains(m2Alt))
    def m2ContainsM1 = m2Context.exists(c => c.contains(m1String) || c.contains(m1Alt))

    m1ContainsM2 || m2ContainsM1
  }
  
  def main(args: Array[String]): Unit = {
    
    val out = new java.io.PrintStream("traindebug2012.txt")
    
    new MentionPairTrainingData("/scratch/", "2012") foreach { case Labelled(item, label) => 
      out.println(label + "\t" + item)
    }
  }
}