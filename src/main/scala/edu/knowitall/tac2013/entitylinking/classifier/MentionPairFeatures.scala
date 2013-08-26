package edu.knowitall.tac2013.entitylinking.classifier

import edu.knowitall.tac2013.entitylinking.utils.FormattedOutputToHumanReadableOutputConverter
import edu.knowitall.tac2013.entitylinking.FormattedOutput
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

class Mention(val query: KBPQuery,val output: FormattedOutputToHumanReadableOutputConverter) {
  
    import MentionPairFeatures.termVector
     
  def entityString = query.name 
  def entityStringUsed = output.entityStringUsed
  def linkId = output.linkId
    
  lazy val stdVec = termVector(query.sourceContext)
  lazy val wideVec = termVector(query.sourceWideContext)
  lazy val corefVec = termVector(query.corefSourceContext.mkString(" "))
  
  private val fulSpec = "[A-Z]\\w+\\s+[A-Z]\\w+".r
  
  def looksFullySpecified =  fulSpec.pattern.matcher(entityStringUsed).matches
}

object Mention {
  
  def from(q: KBPQuery, out: FormattedOutputToHumanReadableOutputConverter): Mention = {
    new Mention(q, out)
  }
}

class MentionPair(val m1: Mention, val m2: Mention) {
  
  import MentionPairFeatures.cosineSimilarity
  
  lazy val standardContextSim = cosineSimilarity(m1.stdVec, m2.stdVec)
  lazy val wideContextSim = cosineSimilarity(m1.wideVec, m2.wideVec)
  lazy val corefContextSim = cosineSimilarity(m1.corefVec, m2.corefVec)
  lazy val alternateStringSim = JaroWinklerMetric.compare(m1.entityStringUsed, m2.entityStringUsed).getOrElse(0.0)
  lazy val entityStringSim = JaroWinklerMetric.compare(m1.entityString, m2.entityString).getOrElse(0.0)
}
object MentionPair {
  def allPairs(ms: Seq[Mention]) = ms.zipWithIndex.flatMap { case (m1, index) =>
    ms.drop(index + 1).map { m2 => 
      val label = m1.linkId == m2.linkId
      new MentionPair(m1, m2) 
    }  
  }
}

object MentionPairFeatures {

  type MentionPairFeature = Feature[MentionPair, Double]
  
  object StandardContextSim extends MentionPairFeature("Std Context Similarity") {
    def apply(pair: MentionPair): Double = {
      pair.standardContextSim
    }
  }
  
  object WideContextSim extends MentionPairFeature("Wide Context Similarity") {
    def apply(pair: MentionPair): Double = {
      pair.wideContextSim
    }
  }
  
  object CorefContextSim extends MentionPairFeature("Coref Context Similarity") {
    def apply(pair: MentionPair): Double = {
      pair.corefContextSim
    }
  }
  
  object AlternateStringSim extends MentionPairFeature("String Similarity of Alternates") {
    def apply(pair: MentionPair): Double = {
      pair.alternateStringSim
    }
  }
  
  object EntityStringSim extends MentionPairFeature("String Similarity of Given Entity String") {
    def apply(pair: MentionPair): Double = {
      pair.entityStringSim
    }
  }
  
  private def features = Seq(StandardContextSim, WideContextSim, CorefContextSim, AlternateStringSim, EntityStringSim)
  
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

  def termVector(str: String): Map[String, Int] = {
    def lemmatize(ts: Seq[PostaggedToken]) = ts.map(stemmer.stemPostaggedToken)
    def clean(ts: Seq[PostaggedToken]) = lemmatize(ts).filter(t => !stopWords.contains(t.lemma)).filter(!_.isPunctuation)
    clean(postagger.postag(str)).groupBy(_.string).map(p => (p._1, p._2.size))
  }
  
  def cosineSimilarity(vec1: Map[String, Int], vec2: Map[String, Int]): Double = {
    val dotProduct = vec1.keys.toSeq.map({ k =>
      val v1 = vec1(k)
      val v2 = vec2.getOrElse(k, 0)
      v1 * v2
    }).sum
    
    val t1norm = math.sqrt(vec1.values.map(v => v*v).sum)
    val t2norm = math.sqrt(vec2.values.map(v => v*v).sum)
    
    dotProduct.toDouble / (t1norm * t2norm)
  }
}

object Foo extends App {
  import MentionPairFeatures._
  
  val s1 = "Thomas Edison invented the light bulb."
  val s2 = "Thomas Edison was the inventor of the light bulb."
    
  println(cosineSimilarity(termVector(s1), termVector(s2)))
}