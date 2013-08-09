package edu.knowitall.tac2013.entitylinking.utils
import edu.knowitall.tac2013.entitylinking.KBPQuery.parseKBPQueries
import java.io.PrintWriter
import java.io.FileWriter
import edu.knowitall.tac2013.entitylinking.KBPQuery
import java.io.File
import edu.knowitall.tac2013.entitylinking.SolrHelper
import edu.knowitall.tac2013.entitylinking.coref.CorefHelperMethods.identifyBestEntityStringByRules

object EntityStringFinder {
  
  def main(args : Array[String]){
	    val queries = parseKBPQueries(getClass.getResource("/edu/knowitall/tac2013/entitylinking/tac_2012_kbp_english_evaluation_entity_linking_queries.xml").getPath()).toSeq
	    val pw = new PrintWriter(new File("alternateEntityStrings.txt"))
	    pw.close()
	    for(q <- queries){
	       val fw = new FileWriter("alternateEntityStrings.txt",true)
	       val alternateEntity = identifyBestEntityStringByRules(q)
	       fw.write(q.id+"\t"+q.doc+"\t"+q.name+"\t"+alternateEntity)
	       fw.close()
	    }
  }

}