package edu.knowitall.tac2013.entitylinking.coref
import edu.knowitall.tac2013.entitylinking.KBPQuery
import java.io.PrintWriter
import java.io.FileWriter
import java.io.File
import edu.knowitall.tac2013.entitylinking.KBPQuery
import edu.knowitall.tac2013.entitylinking.SolrHelper
import scala.collection.mutable
import edu.knowitall.tac2013.entitylinking.utils.XMLHelper
import java.util.Timer
import java.util.concurrent.TimeUnit


object SerializeCorefOffsetsData {
  
  private val cache = new mutable.HashMap[(String, String), SerializeCorefOffsetsData] with mutable.SynchronizedMap[(String, String), SerializeCorefOffsetsData]
  
  def get(basePath: String, year: String) = cache.getOrElseUpdate((basePath, year), new SerializeCorefOffsetsData(basePath, year))
  
  def main(args: Array[String]) {
    val basePath = args(0)
    val year = args(1)
    val queries = KBPQuery.getHelper(basePath, year).parseKBPQueries(getClass.getResource("/edu/knowitall/tac2013/entitylinking/tac_" + year + "_kbp_english_evaluation_entity_linking_queries.xml").getPath()).toSeq
    val pw = new PrintWriter(new File("corefmentions.txt"))
    pw.close()
    for (q <- queries) {
      val fw = new FileWriter("corefmentions.txt", true)
      fw.write(q.id)
      val corefIntervals = KBPQuery.getHelper(basePath, year).corefHelper.getCorefIntervals(SolrHelper.getRawDoc(q.doc,year), q.begOffset, q.endOffset)
      for (cmi <- scala.collection.JavaConversions.asScalaIterable(corefIntervals)) {
        fw.write("\t" + cmi)
      }
      fw.write("\n")
      fw.close()
    }
  }
}

class SerializeCorefOffsetsData(basePath: String, year: String) {



  def serializeCorefOffsetsData {
    //val queries = KBPQuery.getHelper(basePath, year, true).parseKBPQueries(getClass.getResource("/edu/knowitall/tac2013/entitylinking/tac_" + year + "_kbp_english_evaluation_entity_linking_queries.xml").getPath()).toSeq
    val queries = XMLHelper.parseKBPQueryXML(getClass.getResource("/edu/knowitall/tac2013/entitylinking/tac_" + year + "_kbp_english_evaluation_entity_linking_queries.xml").getPath(), year)
    val pw = new PrintWriter(new File("./src/main/resources/edu/knowitall/tac2013/entitylinking/coref/" + year + "corefmentions.txt"))
    pw.close()
    for (q <- queries) {
      val fw = new FileWriter("./src/main/resources/edu/knowitall/tac2013/entitylinking/coref/" + year + "corefmentions.txt", true)
      fw.write(q.id)
      var begOffset = q.begOffSet
      var endOffset = q.endOffset
      val rawDoc = SolrHelper.getRawDoc(q.doc,q.year)
      if (q.begOffSet == -1) {
        begOffset = rawDoc.indexOf(q.name)
      }
      if (q.endOffset == -1) {
        endOffset = rawDoc.indexOf(q.name)
      }
      try{
        val corefIntervals = KBPQuery.getHelper(basePath, year, true).corefHelper.getCorefIntervals(rawDoc, begOffset, endOffset)
        for (cmi <- scala.collection.JavaConversions.asScalaIterable(corefIntervals)) {
          fw.write("\t" + cmi)
        }
      }
      catch{
        case e: Exception => {
        	TimeUnit.MINUTES.sleep(1);
        }
      }
      finally{
       fw.write("\n")
       fw.close()
      }
    }
  }

}