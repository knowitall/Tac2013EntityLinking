package edu.knowitall.tac2013.entitylinking.coref

import edu.knowitall.tac2013.entitylinking.KBPQuery
import java.io.PrintWriter
import java.io.FileWriter
import java.io.File
import edu.knowitall.tac2013.entitylinking.KBPQuery
import edu.knowitall.tac2013.entitylinking.SolrHelper

object SerializeNamedEntities {
  def main(args: Array[String]) {
    val baseDir = args(0)
    val year = args(1)
    val kbpQueryHelper = KBPQuery.getHelper(baseDir, year)
    val queries = kbpQueryHelper.parseKBPQueries(getClass.getResource("/edu/knowitall/tac2013/entitylinking/tac_" + year + "_kbp_english_evaluation_entity_linking_queries.xml").getPath()).toSeq
    val pw = new PrintWriter(new File("namedEntities.txt"))
    pw.close()
    for (q <- queries) {
      val rawDoc = SolrHelper.getRawDoc(q.doc)
      val fw = new FileWriter("namedEntities.txt", true)
      fw.write(q.id)
      val entityTypeList = kbpQueryHelper.corefHelper.getMatchingNamedEntities(rawDoc, q.begOffset)
      for (entityType <- scala.collection.JavaConversions.asScalaIterable(entityTypeList)) {
        fw.write("\t" + entityType)
      }
      fw.write("\n")
      val organizationList = scala.collection.JavaConversions.asScalaIterable(kbpQueryHelper.corefHelper.getNamedEntitiesByType("ORGANIZATION", rawDoc))
      val personList = scala.collection.JavaConversions.asScalaIterable(kbpQueryHelper.corefHelper.getNamedEntitiesByType("PERSON", rawDoc))
      val locationList = scala.collection.JavaConversions.asScalaIterable(kbpQueryHelper.corefHelper.getNamedEntitiesByType("LOCATION", rawDoc))
      fw.write("\tORGANIZATION")
      for (org <- organizationList) {
        fw.write("\t" + org)
      }
      fw.write("\n")
      fw.write("\tLOCATION")
      for (loc <- locationList) {
        fw.write("\t" + loc)
      }
      fw.write("\n")
      fw.write("\tPERSON")
      for (per <- personList) {
        fw.write("\t" + per)
      }
      fw.write("\n")
      fw.close()
    }
  }
}
class SerializeNamedEntities(val baseDir: String, val year: String) {

  val kbpQueryHelper = KBPQuery.getHelper(baseDir, year)

  def serializeNamedEntities {
    val queries = kbpQueryHelper.parseKBPQueries(getClass.getResource("/edu/knowitall/tac2013/entitylinking/tac_" + year + "_kbp_english_evaluation_entity_linking_queries.xml").getPath()).toSeq
    val pw = new PrintWriter(new File("./src/main/resources/edu/knowitall/tac2013/entitylinking/coref/" + year + "namedEntities.txt"))
    pw.close()
    for (q <- queries) {
      val rawDoc = SolrHelper.getRawDoc(q.doc)
      val fw = new FileWriter("./src/main/resources/edu/knowitall/tac2013/entitylinking/coref/" + year + "namedEntities.txt", true)
      fw.write(q.id)
      var offset = q.begOffset
      if (q.begOffset == -1) {
        offset = rawDoc.indexOf(q.name)
      }
      val entityTypeList = kbpQueryHelper.corefHelper.getMatchingNamedEntities(rawDoc, offset)
      for (entityType <- scala.collection.JavaConversions.asScalaIterable(entityTypeList)) {
        fw.write("\t" + entityType)
      }
      fw.write("\n")
      val organizationList = scala.collection.JavaConversions.asScalaIterable(kbpQueryHelper.corefHelper.getNamedEntitiesByType("ORGANIZATION", rawDoc))
      val personList = scala.collection.JavaConversions.asScalaIterable(kbpQueryHelper.corefHelper.getNamedEntitiesByType("PERSON", rawDoc))
      val locationList = scala.collection.JavaConversions.asScalaIterable(kbpQueryHelper.corefHelper.getNamedEntitiesByType("LOCATION", rawDoc))
      fw.write("\tORGANIZATION")
      for (org <- organizationList) {
        fw.write("\t" + org)
      }
      fw.write("\n")
      fw.write("\tLOCATION")
      for (loc <- locationList) {
        fw.write("\t" + loc)
      }
      fw.write("\n")
      fw.write("\tPERSON")
      for (per <- personList) {
        fw.write("\t" + per)
      }
      fw.write("\n")
      fw.close()
    }
  }
}