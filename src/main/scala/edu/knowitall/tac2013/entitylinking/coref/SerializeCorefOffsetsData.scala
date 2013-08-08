package edu.knowitall.tac2013.entitylinking.coref
import edu.knowitall.tac2013.entitylinking.KBPQuery.parseKBPQueries
import java.io.PrintWriter
import java.io.FileWriter
import java.io.File
import edu.knowitall.tac2013.entitylinking.KBPQuery
import edu.knowitall.tac2013.entitylinking.SolrHelper

object SerializeCorefOffsetsData {
 
    def main(args: Array[String]) {
	    val queries = parseKBPQueries(getClass.getResource("tac_2012_kbp_english_evaluation_entity_linking_queries.xml").getPath()).toSeq
	    val pw = new PrintWriter(new File("corefmentions.txt"))
	    pw.close()
	    for(q <- queries){
	       val fw = new FileWriter("corefmentions.txt",true)
	    	  fw.write(q.id)
	          val corefIntervals = KBPQuery.corefHelper.getCorefMentions(SolrHelper.getRawDoc(q.doc),q.begOffset)
	          for(cmi <- scala.collection.JavaConversions.asScalaIterable(corefIntervals)){
	             fw.write("\t" +cmi)
	          }
	    	   fw.write("\n")
	    	fw.close()
	    }
    }

}