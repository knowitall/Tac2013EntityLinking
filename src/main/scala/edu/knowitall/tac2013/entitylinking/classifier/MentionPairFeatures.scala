package edu.knowitall.tac2013.entitylinking.classifier

import edu.knowitall.tac2013.entitylinking.utils.FormattedOutputToHumanReadableOutputConverter
import edu.knowitall.tac2013.entitylinking.KBPQuery
import edu.knowitall.tool.postag.ClearPostagger
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.tool.postag.PostaggedToken
import edu.knowitall.tool.conf.Feature
import edu.knowitall.tool.conf.FeatureSet
import org.apache.lucene.document.Document
import org.apache.lucene.document.Field
import scala.collection.immutable.SortedMap
import com.rockymadden.stringmetric.similarity.JaroWinklerMetric

class Mention(val query: KBPQuery, val output: FormattedOutputToHumanReadableOutputConverter)

class MentionPair(val m1: Mention, val m2: Mention)

object MentionPairFeatures {

  type MentionPairFeature = Feature[MentionPair, Double]
  
  object StandardContextSim extends MentionPairFeature("Std Context Similarity") {
    def apply(pair: MentionPair): Double = {
      cosineSimilarity(pair.m1.query.sourceContext, pair.m2.query.sourceContext)
    }
  }
  
  object WideContextSim extends MentionPairFeature("Wide Context Similarity") {
    def apply(pair: MentionPair): Double = {
      cosineSimilarity(pair.m1.query.sourceWideContext, pair.m2.query.sourceWideContext)
    }
  }
  
  object CorefContextSim extends MentionPairFeature("Coref Context Similarity") {
    def apply(pair: MentionPair): Double = {
      cosineSimilarity(pair.m1.query.sourceWideContext, pair.m2.query.sourceWideContext)
    }
  }
  
  object AlternateStringSim extends MentionPairFeature("String Similarity of Alternates") {
    def apply(pair: MentionPair): Double = {
      JaroWinklerMetric.compare(pair.m1.output.entityStringUsed, pair.m2.output.entityStringUsed).getOrElse(0.0)
    }
  }
  
  object EntityStringSim extends MentionPairFeature("String Similarity of Given Entity String") {
    def apply(pair: MentionPair): Double = {
      JaroWinklerMetric.compare(pair.m1.query.name, pair.m2.query.name).getOrElse(0.0)
    }
  }
  
  private def features = Seq(StandardContextSim, WideContextSim, CorefContextSim)
  
  private def sortedFeatureMap = SortedMap.empty[String, MentionPairFeature] ++ features.map(f => (f.name, f)).toMap
  
  def featureSet = new FeatureSet(sortedFeatureMap)
  
  // Helper Methods Below this point.
  
  private val postagger = new ClearPostagger
  private val stemmer = new MorphaStemmer
  
  private val stopWords = {
    val url = getClass.getResource("stopwords.txt")
    require(url != null, "Could not find stopwords.txt")
    io.Source.fromURL(url, "UTF8").getLines.flatMap(_.split(",")).map(_.toLowerCase).toSet
  }
  
  def cosineSimilarity(words1: String, words2: String): Double = {
    cosineSimilarity(postagger(words1), postagger(words2))
  }
  
  def cosineSimilarity(tokens1: Seq[PostaggedToken], tokens2: Seq[PostaggedToken]): Double = {
    
    def lemmatize(ts: Seq[PostaggedToken]) = ts.map(stemmer.stemPostaggedToken)
    def clean(ts: Seq[PostaggedToken]) = lemmatize(ts).filter(t => !stopWords.contains(t.lemma)).filter(!_.isPunctuation)
    
    val t1Freqs = clean(tokens1).groupBy(_.string).map(p => (p._1, p._2.size))
    val t2Freqs = clean(tokens2).groupBy(_.string).map(p => (p._1, p._2.size))
    val dotProduct = t1Freqs.keys.toSeq.map({ k =>
      val v1 = t1Freqs(k)
      val v2 = t2Freqs.getOrElse(k, 0)
      v1 * v2
    }).sum
    
    val t1norm = math.sqrt(t1Freqs.values.map(v => v*v).sum)
    val t2norm = math.sqrt(t2Freqs.values.map(v => v*v).sum)
    
    dotProduct.toDouble / (t1norm * t2norm)
  }
}

object Foo extends App {
  import MentionPairFeatures.cosineSimilarity
  
  val s1 = "Thomas Edison invented the light bulb."
  val s2 = "Thomas Edison was the inventor of the light bulb."
    
  println(cosineSimilarity(s1, s2))
}