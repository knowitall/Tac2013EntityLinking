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
import edu.knowitall.tac2013.entitylinking.utils.FormattedOutputToHumanReadableOutputConverter
import edu.knowitall.tac2013.entitylinking.coref.CorefHelperMethods.identifyBestEntityStringByRules
import edu.knowitall.tac2013.entitylinking.RunKBPEntityLinkerSystem
import edu.knowitall.tac2013.entitylinking.utils.FormattedOutputToHumanReadableOutputConverter
import edu.knowitall.tool.conf.Labelled

object MentionPairTrainingData extends Iterable[Labelled[MentionPair]] {

  KBPQuery.activate("/scratch/")
    
  private val linker = RunKBPEntityLinkerSystem.linker
  
  private val linkClassifier = RunKBPEntityLinkerSystem.linkClassifier
  
  
  private val queries = parseKBPQueries(getClass.getResource("/edu/knowitall/tac2013/entitylinking/tac_2012_kbp_english_evaluation_entity_linking_queries.xml").getPath()).toSeq
  private val queryAnswers = queries.map(q => (q, RunKBPEntityLinkerSystem.linkQuery(q, linker, linkClassifier)))
  private val mentions = queryAnswers.map { case (query, answer) =>
    val humanReadable = new FormattedOutputToHumanReadableOutputConverter(answer, query)
    new Mention(query, humanReadable)
  }
  
  private val mentionPairs = mentions.zipWithIndex.flatMap { case (m1, index) =>
    mentions.drop(index + 1).map { m2 => 
      val label = m1.output.linkId == m2.output.linkId
      Labelled(label, new MentionPair(m1, m2)) 
    }  
  }
  
  def iterator = mentionPairs.iterator
}

object MPTest extends App {
  val size = MentionPairTrainingData.size
  println("num examples: " + size)
  val positive = MentionPairTrainingData.count(_.label == true)
  println("num positive: " + positive)
  println("num negative: " + (size - positive))
}