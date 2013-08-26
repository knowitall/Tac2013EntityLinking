package edu.knowitall.tac2013.entitylinking.utils
import edu.knowitall.tac2013.entitylinking.KBPQuery
import java.io.PrintWriter
import java.io.FileWriter
import edu.knowitall.tac2013.entitylinking.KBPQuery
import java.io.File
import edu.knowitall.tac2013.entitylinking.SolrHelper
import edu.knowitall.tac2013.entitylinking.coref.CorefHelperMethods

object EntityStringFinder {
  
  def main(args : Array[String]){
    val baseDir = args(0)
    val year = args(1)
	    val queries = KBPQuery.getHelper(baseDir, year).parseKBPQueries(getClass.getResource("/edu/knowitall/tac2013/entitylinking/tac_2012_kbp_english_evaluation_entity_linking_queries.xml").getPath()).toSeq
	    val pw = new PrintWriter(new File("alternateEntityStrings.txt"))
	    for(q <- queries){
	       val alternateEntity = CorefHelperMethods.get(year).identifyBestEntityStringByRules(q)
	       pw.write(q.id+"\t"+q.doc+"\t"+q.name+"\t"+alternateEntity+"\n")
	    }
	    pw.close()
  }

}