package edu.knowitall.tac2013.entitylinking.coref
import edu.knowitall.tac2013.entitylinking.KBPQuery.parseKBPQueries
import java.io.PrintWriter
import java.io.FileWriter
import java.io.File
import edu.knowitall.tac2013.entitylinking.KBPQuery
import edu.knowitall.tac2013.entitylinking.SolrHelper
  
object SerializeCorefStringMentions {
    def main(args: Array[String]) {
	    val queries = parseKBPQueries(getClass.getResource("/edu/knowitall/tac2013/entitylinking/tac_2012_kbp_english_evaluation_entity_linking_queries.xml").getPath()).toSeq
	    val pw = new PrintWriter(new File("corefStringMentions.txt"))
	    pw.close()
	    for(q <- queries){
	       val fw = new FileWriter("corefStringMentions.txt",true)
	    	  fw.write(q.id)
	    	  val mentions  = KBPQuery.corefHelper.getCorefStringMentions(SolrHelper.getRawDoc(q.doc),q.begOffset)
	    	  if(!mentions.isEmpty()){
	    	    val uniqueMentions = scala.collection.JavaConversions.asScalaIterable(mentions).toSet.toList
	    	    for(um <- uniqueMentions)
	              fw.write("\t" + um)
	    	  }
	    	  fw.write("\n")
	    	fw.close()
	    }
    }
}