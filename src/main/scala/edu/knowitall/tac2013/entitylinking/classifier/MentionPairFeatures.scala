package edu.knowitall.tac2013.entitylinking.classifier

import edu.knowitall.tac2013.entitylinking.utils.FormattedOutputToHumanReadableOutputConverter
import edu.knowitall.tac2013.entitylinking.ExtrSolrHelper
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
import edu.knowitall.common.Resource.using

class Mention(val query: KBPQuery,val output: FormattedOutputToHumanReadableOutputConverter) {
  
    import MentionPairFeatures.termVector
    import Mention.commonSurnames
     
  def entityString = query.name 
  def entityStringUsed = output.entityStringUsed
  val surfaceNames = Set(entityString.toLowerCase, entityStringUsed.toLowerCase)
  def toAbbr(str: String) = str.split("\\s+").map(_.take(1)).map(_.toUpperCase).mkString
  val abbrs = surfaceNames.map(toAbbr)
  
  def linkId = output.linkId
    
  lazy val stdVec = termVector(query.sourceContext)
  lazy val wideVec = termVector(query.sourceWideContext)
  lazy val corefVec = termVector(query.corefSourceContext.mkString(" "))
  lazy val linkVec = ExtrSolrHelper.get(query.year).getNodeIds(query.doc).groupBy(identity).map(p => (p._1, p._2.size))
  lazy val isCommonSurname = surfaceNames.exists(commonSurnames.contains _)
  lazy val argVec = termVector(ExtrSolrHelper.get(query.year).getExtrArgs(query.doc).mkString(" "))
  
  private val fulSpec = "[A-Z]\\w+\\s+[A-Z]\\w+".r
  
  def looksFullySpecified =  fulSpec.pattern.matcher(entityStringUsed).matches
}

object Mention {
  
  def from(q: KBPQuery, out: FormattedOutputToHumanReadableOutputConverter): Mention = {
    new Mention(q, out)
  }
  
  private val surnameResourceName = "common-surnames.txt" 
  private val surnameResource = Option(getClass.getResource(surnameResourceName)).getOrElse(throw new RuntimeException("Couldn't find "+surnameResourceName))
  val commonSurnames = using(io.Source.fromURL(surnameResource)) { source =>
    val names = source.getLines.map(_.takeWhile(_ != '\t'))
    names.map(_.trim.toLowerCase).toSet
  }
}

class MentionPair(val m1: Mention, val m2: Mention) {
  
  import MentionPairFeatures.cosineSimilarity
  
  lazy val standardContextSim = cosineSimilarity(m1.stdVec, m2.stdVec)
  lazy val wideContextSim = cosineSimilarity(m1.wideVec, m2.wideVec)
  lazy val corefContextSim = cosineSimilarity(m1.corefVec, m2.corefVec)
  lazy val alternateStringSim = JaroWinklerMetric.compare(m1.entityStringUsed, m2.entityStringUsed).getOrElse(0.0)
  lazy val entityStringSim = JaroWinklerMetric.compare(m1.entityString, m2.entityString).getOrElse(0.0)
  val namesOverlap = m1.surfaceNames.intersect(m2.surfaceNames).nonEmpty
  lazy val sameCommonSurname = m1.isCommonSurname && m2.isCommonSurname && namesOverlap
  lazy val prefix = m1.surfaceNames.flatMap(n1 => m2.surfaceNames.map(n2 => (n1, n2))).exists(np => np._1.startsWith(np._2) || np._2.startsWith(np._1))
  lazy val contains = m1.surfaceNames.flatMap(n1 => m2.surfaceNames.map(n2 => (n1, n2))).exists(np => np._1.contains(np._2) || np._2.contains(np._1)) && !namesOverlap
  lazy val abbrPresent = m1.surfaceNames.exists(n1 => m2.abbrs.contains(n1)) || m2.surfaceNames.exists(n2 => m1.abbrs.contains(n2))
  lazy val linkSim = cosineSimilarity(m1.linkVec, m2.linkVec)
  lazy val argSim = cosineSimilarity(m1.argVec, m2.argVec)
  
  override def toString: String = {
    val n1 = m1.entityStringUsed
    val n2 = m2.entityStringUsed
    val doubles = Seq(
        standardContextSim,  
        corefContextSim, 
        alternateStringSim, 
        linkSim,
        argSim).map(d => "%.03f".format(d))
    val booleans = Seq(
        sameCommonSurname, 
        prefix, 
        contains).map(b => if (b) "true" else "false")
    (Seq(n1, n2) ++ doubles ++ booleans).mkString("\t")
  }
  
}
object MentionPair {

  val header = Seq("Name1", "Name2",
    "std context",
    "coref context",
    "alt string sim",
    "link cosine sim",
    "arg cosine sim",
    "same common surname",
    "name prefix of other",
    "name contains other").mkString("\t")
  
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
      (pair.standardContextSim + pair.wideContextSim) / 2
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
  
  object ExtrArgSim extends MentionPairFeature("Extraction Argument Cosine Similarity") {
    def apply(pair: MentionPair): Double = {
      pair.argSim
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
  
  object SameCommonSurname extends MentionPairFeature("Both are (the same) common surname") {
    def apply(pair: MentionPair): Double = {
      if (pair.sameCommonSurname) 1.0 else 0.0
    }
  }
  
  object Prefix extends MentionPairFeature("A name is prefix of other") {
    def apply(pair: MentionPair): Double = {
      if (pair.prefix) 1.0 else 0.0
    }
  }
    
  object Contains extends MentionPairFeature("A name contains other") {
    def apply(pair: MentionPair): Double = {
      if (pair.contains) 1.0 else 0.0
    }
  }
      
  object Abbr extends MentionPairFeature("A name is ABBR of other") {
    def apply(pair: MentionPair): Double = {
      if (pair.abbrPresent) 1.0 else 0.0
    }
  }
  
  object LinkSimilarity extends MentionPairFeature("Link Cosine Similarity") {
    def apply(pair: MentionPair): Double = {
      pair.linkSim
    }
  }
  
  private def features = Seq(
      StandardContextSim,        
      CorefContextSim, 
      AlternateStringSim, 
      SameCommonSurname,
      Prefix,
      Contains,
      LinkSimilarity,
      ExtrArgSim)
  
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
    
    val cosineSim = dotProduct.toDouble / (t1norm * t2norm)
    if (cosineSim.isNaN() || cosineSim.isInfinity) 0.0 else cosineSim
  }
}

object Foo extends App {
  import MentionPairFeatures._
  
  val s1 = "Thomas Edison invented the light bulb."
  val s2 = "Thomas Edison was the inventor of the light bulb."
    
  println(cosineSimilarity(termVector(s1), termVector(s2)))
}