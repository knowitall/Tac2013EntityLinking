package edu.knowitall.tac2013.entitylinking

import edu.knowitall.browser.entity.EntityLinker
import edu.knowitall.browser.entity.batch_match
import edu.knowitall.browser.entity.StringMatchCandidateFinder
import edu.knowitall.browser.entity.CrosswikisCandidateFinder
import edu.knowitall.browser.entity.EntityTyper
import edu.knowitall.common.Resource.using
import edu.knowitall.tac2013.entitylinking.utils.WikiMappingHelper
import scopt.OptionParser
import edu.knowitall.tac2013.entitylinking.utils.FormattedOutputToHumanReadableOutputConverter
import edu.knowitall.tac2013.entitylinking.classifier.LinkClassifier
import edu.knowitall.tac2013.entitylinking.classifier.MentionPairClassifier
import edu.knowitall.tac2013.entitylinking.classifier.Mention
import edu.knowitall.tac2013.entitylinking.classifier.MentionPair
import edu.knowitall.tac2013.entitylinking.utils.ResourceHelper
import edu.knowitall.tac2013.entitylinking.classifier.MentionPairTrainingData.mentionPairFilter

import scala.collection.mutable.HashMap


object Clusterer {
  
  private val cutoff= 0.9999821007

  
  private var mentionPairCache = new HashMap[(String, String), Option[MentionPair]]
  val mpClassifier = MentionPairClassifier.default
  
  def pairwiseClusterNils(answers: Seq[FormattedOutput], queries: Seq[KBPQuery]) :Seq[FormattedOutput] = {
    // get all the mentions 
    val mentions = answers.zip(queries).map(p => Mention.from(p._2, new FormattedOutputToHumanReadableOutputConverter(p._1, p._2)))
    // get a map of link id's to clusters
    var clusters = mentions.groupBy(_.linkId)
    println(s"Starting with ${clusters.size} clusters")
    // iteratively, compute similarity between all pairs of clusters and merge the two most similar
    var done = false
    
    while (!done) {
      val clusterPairs = allDistinctPairs(clusters.keys.toSeq).filter(p => p._1.startsWith("NIL") || p._2.startsWith("NIL"))
      print(s"Computing similarity for ${clusterPairs.size} cluster pairs")
      val clusterPairSimilarities = clusterPairs.zipWithIndex.map { case ((id1, id2), index) => 
        if (index % 100000 == 0) print(".")
        val similarity = clusterSimilarity(clusters(id1), clusters(id2))
        ((id1, id2), similarity)      
      }
      val ((ms1, ms2), maxSim) = clusterPairSimilarities.maxBy(_._2)
      if (maxSim < cutoff) done = true
      else {
        val merg1 = clusters(ms1)
        val merg2 = clusters(ms2)
        val m1Names = merg1.map(_.entityStringUsed).mkString("(", ", ", ")")
        val m2Names = merg2.map(_.entityStringUsed).mkString("(", ", ", ")")
        println(s"Merging $ms1 and $ms2 $m1Names and $m2Names")
        val mergedMentions = clusters(ms1) ++ clusters(ms2)
        if (!ms2.startsWith("E")) {
          clusters -= ms2
          clusters += (ms1 -> mergedMentions)
        } else {
          clusters -= ms1
          clusters += (ms2 -> mergedMentions)
        }
      }
    }
    
    println(s"Ending with ${clusters.size} clusters")
    
    clusters.iterator.toSeq.flatMap { case (id, mentions) =>
      mentions.map(_.output.formattedOutput.copy(kbLink = id))
    }
  }
  
  def clusterSimilarity(c1: Seq[Mention], c2: Seq[Mention]): Double = {
    val c1Queries = c1.map(m => (m.output.queryId, m)).toMap
    val c2Queries = c2.map(m => (m.output.queryId, m)).toMap
    val queryPairs = c1Queries.keysIterator.flatMap(c1 => c2Queries.keysIterator.map(c2 => (c1, c2)))
    val mentionPairs = queryPairs.flatMap({ qp => 
      def update = {
        val mp = new MentionPair(c1Queries(qp._1), c2Queries(qp._2))
        Some(mp)
      }
      mentionPairCache.getOrElseUpdate(qp, update)
    }).toSeq
    val similarities = mentionPairs.map(mpClassifier.score)
    if (similarities.nonEmpty) similarities.sum / similarities.size.toDouble
    else 0.0
  }
  
  def allDistinctPairs[T](seq: Seq[T]): Seq[(T, T)] = {
    seq.zipWithIndex.flatMap { case (t1, index) =>
      seq.drop(index + 1).map(t2 => (t1, t2))  
    }
  }
}