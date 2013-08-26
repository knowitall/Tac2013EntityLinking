package edu.knowitall.tac2013.entitylinking.classifier

import edu.knowitall.tac2013.entitylinking.KBPQuery
import KBPQuery.parseKBPQueries
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
import edu.knowitall.tac2013.entitylinking.coref.CorefHelperMethods.identifyBestEntityStringByRules
import edu.knowitall.tac2013.entitylinking.RunKBPEntityLinkerSystem
import edu.knowitall.tac2013.entitylinking.utils.FormattedOutputToHumanReadableOutputConverter
import edu.knowitall.tool.conf.Labelled

class MentionPairTrainingData(val basePath: String, val year: String) extends Iterable[Labelled[MentionPair]] {
    
  KBPQuery.activate(basePath, year)
  
  val answerUrl = getClass.getResource("/edu/knowitall/tac2013/entitylinking/benchmark/tac_"+year+"_kbp_english_evaluation_entity_linking_query_types.tab")
  val answers = using(io.Source.fromURL(answerUrl, "UTF8")) { answerSrc => answerSrc.getLines.map(FormattedOutput.readFormattedOutput).toList }
  val answersMap = answers.map(a => (a.queryId, a)).toMap
  
  private val queries = parseKBPQueries(getClass.getResource("/edu/knowitall/tac2013/entitylinking/tac_"+year+"_kbp_english_evaluation_entity_linking_queries.xml").getPath()).toSeq
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

  private val filteredMentionPairs = allMentionPairs.filter { mp =>
    MentionPairTrainingData.mentionPairFilter(mp.item)
  }
  
  private val rand = new scala.util.Random(0)
  
  private val positiveFilteredPairs = filteredMentionPairs.filter(_.label)
  private val negativeFilteredPairs = rand.shuffle(filteredMentionPairs.filterNot(_.label))
  private val positivePairs = allMentionPairs.filter(_.label)
  private val negativePairs = allMentionPairs.filterNot(_.label)
  
  
  def iterator = rand.shuffle(positiveFilteredPairs ++ negativeFilteredPairs.take(positiveFilteredPairs.size * 15)).iterator
}

object MentionPairTrainingData {
  def mentionPairFilter(mp: MentionPair): Boolean = {
    val m1 = mp.m1
    val m2 = mp.m2
    def m1Context = (m1.query.sourceWideContext :: m1.query.corefSourceContext).map(_.toLowerCase())
    def m2Context = (m2.query.sourceWideContext :: m2.query.corefSourceContext).map(_.toLowerCase())
    val m1String = m1.query.entityString.toLowerCase
    val m2String = m2.query.entityString.toLowerCase
    val m1Alt = m1.output.entityStringUsed.toLowerCase
    val m2Alt = m2.output.entityStringUsed.toLowerCase
    def m1ContainsM2 = m1Context.exists(c => c.contains(m2String) || c.contains(m2Alt))
    def m2ContainsM1 = m2Context.exists(c => c.contains(m1String) || c.contains(m1Alt))

    m1ContainsM2 || m2ContainsM1
  }
}