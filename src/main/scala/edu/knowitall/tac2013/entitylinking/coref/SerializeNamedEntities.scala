package edu.knowitall.tac2013.entitylinking.coref

import edu.knowitall.tac2013.entitylinking.KBPQuery.parseKBPQueries
import java.io.PrintWriter
import java.io.FileWriter
import java.io.File
import edu.knowitall.tac2013.entitylinking.KBPQuery
import edu.knowitall.tac2013.entitylinking.SolrHelper

object SerializeNamedEntities {
    def main(args: Array[String]) {
	    val queries = parseKBPQueries(getClass.getResource("/edu/knowitall/tac2013/entitylinking/tac_2012_kbp_english_evaluation_entity_linking_queries.xml").getPath()).toSeq
	    val pw = new PrintWriter(new File("namedEntities.txt"))
	    pw.close()
	    for(q <- queries){
	       val rawDoc = SolrHelper.getRawDoc(q.doc)
	       val fw = new FileWriter("namedEntities.txt",true)
	    	  fw.write(q.id)
	    	  val entityTypeList  = KBPQuery.corefHelper.getMatchingNamedEntities(rawDoc,q.begOffset)
	    	  for(entityType <- scala.collection.JavaConversions.asScalaIterable(entityTypeList)){
	    	    fw.write("\t" +entityType)
	    	  }
	          fw.write("\n")
	          val organizationList = scala.collection.JavaConversions.asScalaIterable(KBPQuery.corefHelper.getNamedEntitiesByType("ORGANIZATION",rawDoc))
	          val personList = scala.collection.JavaConversions.asScalaIterable(KBPQuery.corefHelper.getNamedEntitiesByType("PERSON",rawDoc))
	          val locationList = scala.collection.JavaConversions.asScalaIterable(KBPQuery.corefHelper.getNamedEntitiesByType("LOCATION", rawDoc))
	          fw.write("\tORGANIZATION")
	          for(org <- organizationList){
	            fw.write("\t"+org)
	          }
	          fw.write("\n")
	          fw.write("\tLOCATION")
	          for(loc <- locationList){
	            fw.write("\t"+loc)
	          }
	          fw.write("\n")
	          fw.write("\tPERSON")
	          for(per <- personList){
	            fw.write("\t"+per)
	          }
	          fw.write("\n")
	    	fw.close()
	    }
    }
    
    def serializeNamedEntities(year: String) {
	    val queries = parseKBPQueries(getClass.getResource("/edu/knowitall/tac2013/entitylinking/tac_"+year+"_kbp_english_evaluation_entity_linking_queries.xml").getPath()).toSeq
	    val pw = new PrintWriter(new File(".src/main/resources/edu/knowitall/tac2013/enitylinking/coref/"+year+"namedEntities.txt"))
	    pw.close()
	    for(q <- queries){
	       val rawDoc = SolrHelper.getRawDoc(q.doc)
	       val fw = new FileWriter(".src/main/resources/edu/knowitall/tac2013/enitylinking/coref/"+year+"namedEntities.txt",true)
	    	  fw.write(q.id)
	    	  val entityTypeList  = KBPQuery.corefHelper.getMatchingNamedEntities(rawDoc,q.begOffset)
	    	  for(entityType <- scala.collection.JavaConversions.asScalaIterable(entityTypeList)){
	    	    fw.write("\t" +entityType)
	    	  }
	          fw.write("\n")
	          val organizationList = scala.collection.JavaConversions.asScalaIterable(KBPQuery.corefHelper.getNamedEntitiesByType("ORGANIZATION",rawDoc))
	          val personList = scala.collection.JavaConversions.asScalaIterable(KBPQuery.corefHelper.getNamedEntitiesByType("PERSON",rawDoc))
	          val locationList = scala.collection.JavaConversions.asScalaIterable(KBPQuery.corefHelper.getNamedEntitiesByType("LOCATION", rawDoc))
	          fw.write("\tORGANIZATION")
	          for(org <- organizationList){
	            fw.write("\t"+org)
	          }
	          fw.write("\n")
	          fw.write("\tLOCATION")
	          for(loc <- locationList){
	            fw.write("\t"+loc)
	          }
	          fw.write("\n")
	          fw.write("\tPERSON")
	          for(per <- personList){
	            fw.write("\t"+per)
	          }
	          fw.write("\n")
	    	fw.close()
	    }
    }
}