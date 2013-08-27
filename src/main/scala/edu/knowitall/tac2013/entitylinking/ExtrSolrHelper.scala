package edu.knowitall.tac2013.entitylinking

import jp.sf.amateras.solr.scala.SolrClient
import org.apache.solr.common.SolrDocument
import edu.knowitall.tac2013.openie.KbpExtraction
import edu.knowitall.tac2013.openie.KbpArgument
import scala.collection.mutable

case class ExtrSolrHelper private (val year: String) {

  private val extractionClient = year match {
    case "2011" | "2012" => new SolrClient("http://knowitall:knowit!@rv-n16.cs.washington.edu:9321/solr")
    case "2013" => new SolrClient("http://knowitall:knowit!@rv-n16.cs.washington.edu:9321/solr")
    case _ => throw new RuntimeException("No extraction database for year "+ year)
  }
  
  private def execQuery(qString: String): List[Map[String, Any]] = {
    extractionClient.query(qString).rows(10000).getResultAsMap().documents
  }
  
  def getNodeIds(docId: String): Seq[String] = {
    
    val query = s"+docId:$docId"
    execQuery(query).flatMap { doc =>
      val arg1NodeId = doc.get("arg1WikiLinkFbid").get.asInstanceOf[String]
      val arg2NodeId = doc.get("arg2WikiLinkFbid").get.asInstanceOf[String]
      Seq(arg1NodeId, arg2NodeId).filter(_.nonEmpty)
    }
  }
  
  def getExtrArgs(docId: String): Seq[String] = {
    
    val query = s"+docId:$docId"
    execQuery(query).flatMap { doc =>
      val arg1Text = doc.get("arg1Text").get.asInstanceOf[String]
      val arg2Text = doc.get("arg2Text").get.asInstanceOf[String]
      Seq(arg1Text, arg2Text).filter(_.nonEmpty)
    }
  }
  
  def getExtrs(docId: String): Seq[KbpExtraction] = {
    val qString = s"+docId:$docId"
    val extrs = execQuery(qString).flatMap(KbpExtraction.fromFieldMap)
    extrs
  }
  
  def extrNearbyArgs(kbpQuery: KBPQuery): Seq[(String, Int)] = {
    val extrs = getExtrs(kbpQuery.doc)
    val mentionOffset = kbpQuery.begOffset
    val stringOffsets = extrs.flatMap { e =>
      val a1Start = e.sentence.startOffset + e.arg1.tokens.head.offsets.start  
      val a2Start = e.sentence.startOffset + e.arg2.tokens.head.offsets.start
      Seq((e.arg1, a1Start), (e.arg2, a2Start))
    }    
    val withinWindow = stringOffsets.filter(p => math.abs(mentionOffset - p._2) < 500)
    val proper = withinWindow.map(p => (p._1.tokens.filter(t => t.isProperNoun), p._2)).filter(_._1.nonEmpty)
    val toStrings = proper.map(p => (p._1.map(_.string).mkString(" "), p._2))
    val notSameAsMention = toStrings.filterNot(_._1.toLowerCase == kbpQuery.entityString.toLowerCase)
    notSameAsMention.sortBy(_._2)
  }
  
}

object ExtrSolrHelper {
  
  private val cache = new mutable.HashMap[String, ExtrSolrHelper]
  
  def get(year: String): ExtrSolrHelper = cache.getOrElseUpdate(year, ExtrSolrHelper(year))

  def main(args: Array[String]): Unit = {
    
    val helper = KBPQuery.getHelper("/scratch/", "2012")
    
    val queries = helper.parseKBPQueries(getClass.getResource("tac_2012_kbp_english_evaluation_entity_linking_queries.xml").getPath()).toSeq
    
    val qMap = queries.map(q => (q.id, q)).toMap
    
    def qnum(s: String) = qMap(s"EL_ENG_$s")
    
    val qs = Seq("01765", "00219", "00515", "01016").map(qnum)

    qs.foreach { q =>

      println(q.name)
      ExtrSolrHelper.get("2012").extrNearbyArgs(q) foreach println
      println
    }
  }
}