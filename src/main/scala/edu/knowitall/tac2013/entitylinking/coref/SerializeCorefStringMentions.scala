package edu.knowitall.tac2013.entitylinking.coref
import edu.knowitall.tac2013.entitylinking.KBPQuery
import java.io.PrintWriter
import java.io.FileWriter
import java.io.File
import edu.knowitall.tac2013.entitylinking.KBPQuery
import edu.knowitall.tac2013.entitylinking.SolrHelper
  
object SerializeCorefStringMentions {
    def main(args: Array[String]) {
      val baseDir = args(0)
      val year = args(1)
      
	    val queries = KBPQuery.getHelper(baseDir, year).parseKBPQueries(getClass.getResource("/edu/knowitall/tac2013/entitylinking/tac_"+year+"_kbp_english_evaluation_entity_linking_queries.xml").getPath()).toSeq
	    val pw = new PrintWriter(new File("corefStringMentions.txt"))
	    pw.close()
	    for(q <- queries){
	       val fw = new FileWriter("corefStringMentions.txt",true)
	    	  fw.write(q.id)
	    	  val mentions  = KBPQuery.getHelper(baseDir, year).corefHelper.getCorefStringMentions(SolrHelper.getRawDoc(q.doc,q.year),q.begOffset,q.endOffset)
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