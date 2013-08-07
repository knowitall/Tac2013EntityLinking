package edu.knowitall.tac2013.entitylinking

import scopt.OptionParser
import KBPQuery.parseKBPQueries

object SerializeCorefData {
    var baseDir = "/scratch/"
 
    def main(args: Array[String]) {
	    val argParser = new OptionParser() {
	      arg("baseDir", "Path to base directory with entitylinking files.", { s => baseDir = s })
	    }
	
	    if(!argParser.parse(args)) return
	    
	    KBPQuery.activate(baseDir)
	    
	    val queries = parseKBPQueries(getClass.getResource("tac_2012_kbp_english_evaluation_entity_linking_queries.xml").getPath()).toSeq
	    
	    for(q <- queries){
	    	  println(q.id)
	          val corefIntervals = KBPQuery.corefHelper.getCorefMentions(SolrHelper.getRawDoc(q.doc),q.begOffset)
	          for(cmi <- scala.collection.JavaConversions.asScalaIterable(corefIntervals)){
	            println(cmi)
	          }
	    }
    }

}