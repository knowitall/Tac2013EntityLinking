package edu.knowitall.tac2013.entitylinking.coref
import edu.knowitall.tac2013.entitylinking.KBPQuery
import java.io.PrintWriter
import java.io.FileWriter
import java.io.File
import edu.knowitall.tac2013.entitylinking.KBPQuery
import edu.knowitall.tac2013.entitylinking.SolrHelper

object SerializeCorefTypes {
    def main(args: Array[String]) {
      val baseDir = args(0)
      val year = args(1)
	    val queries = KBPQuery.getHelper(baseDir, year).parseKBPQueries(getClass.getResource("/edu/knowitall/tac2013/entitylinking/tac_2012_kbp_english_evaluation_entity_linking_queries.xml").getPath()).toSeq
	    val pw = new PrintWriter(new File("corefTypes.txt"))
	    pw.close()
	    for(q <- queries){
	       val fw = new FileWriter("corefTypes.txt",true)
	    	  fw.write(q.id)
	    	  val entityTypeList  = KBPQuery.getHelper(baseDir, year).corefHelper.getMatchingNamedEntities(SolrHelper.getRawDoc(q.doc,q.year),q.begOffset, q.endOffset)
	    	  for(entityType <- scala.collection.JavaConversions.asScalaIterable(entityTypeList)){
	    	    fw.write("\t" +entityType)
	    	  }
	    	  fw.write("\n")
	    	fw.close()
	    }
    }
}