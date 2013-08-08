package edu.knowitall.tac2013.entitylinking.coref
import edu.knowitall.tac2013.entitylinking.KBPQuery.parseKBPQueries
import java.io.PrintWriter
import java.io.FileWriter
import java.io.File
import edu.knowitall.tac2013.entitylinking.KBPQuery
import edu.knowitall.tac2013.entitylinking.SolrHelper


object SerializeRepresentativeMentionData {
    def main(args: Array[String]) {
	    val queries = parseKBPQueries(getClass.getResource("/edu/knowitall/tac2013/entitylinking/tac_2012_kbp_english_evaluation_entity_linking_queries.xml").getPath()).toSeq
	    val pw = new PrintWriter(new File("corefRepresentativeMentions.txt"))
	    pw.close()
	    for(q <- queries){
	       val fw = new FileWriter("corefRepresentativeMentions.txt",true)
	    	  fw.write(q.id)
	    	  val representativeMention = KBPQuery.corefHelper.getCorefRepresentativeString(SolrHelper.getRawDoc(q.doc),q.begOffset)
	    	  if(representativeMention != null)
	            fw.write("\t" + representativeMention)
	    	  fw.write("\n")
	    	fw.close()
	    }
    }
}