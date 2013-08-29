package edu.knowitall.tac2013.entitylinking.classifier

import edu.knowitall.tool.conf.Feature
import edu.knowitall.tool.conf.FeatureSet
import edu.knowitall.browser.entity.EntityLink
import scala.collection.immutable.SortedMap
import edu.knowitall.tac2013.entitylinking.KBPQuery

class KBPQueryLink(val query: KBPQuery, val link: EntityLink)

object LinkFeatures {
  
  val kbIdToNamedEntitiesMap = {for(line <- scala.io.Source.fromFile("KbNamedEntitiesMap.txt")(scala.io.Codec.UTF8).getLines().toSeq) yield{
    val values = line.split("\t")
    val kbId = values(0)
    val namedEntities = values.slice(1,values.length).toList
    (kbId,namedEntities)
  }
  }.toMap
  

  type LinkFeature = Feature[KBPQueryLink, Double]

  case object candidateScore extends LinkFeature("candidate score") {
    def apply(link: KBPQueryLink) = {
      link.link.candidateScore
    }
  }

  object inlinkScore extends LinkFeature("inlink score") {
    def apply(link: KBPQueryLink) = {
      math.log(link.link.inlinks)
    }
  }

  object docSimScore extends LinkFeature("docsim score") {
    def apply(link: KBPQueryLink) = {
      link.link.docSimScore
    }
  }
  

  
  object originalNameMatch extends LinkFeature("original name Match"){
      def apply(link: KBPQueryLink) = {
	      val query = link.query
	      val kbLink = link.link
	      val kbLinkName = kbLink.entity.name
	      var score = 0.0
	      if(query.name == kbLinkName){
	        score = 1.0
	      }
	      score
      }
  }
  
  object fullNameMatch extends LinkFeature("full name Match"){
      def apply(link: KBPQueryLink) = {
	      val query = link.query
	      val kbLink = link.link
	      val kbLinkName = kbLink.entity.name
	      var score = 0.0
	      if(query.entityString == kbLinkName){
	        score = 1.0
	      }
	      score
      }
  }
  
  object fullNameFound extends LinkFeature("full name Found"){
    def apply(link: KBPQueryLink) = {
      val queryName = link.query.name
      val altName = link.query.entityString
      if( queryName != altName){
        1.0
      }
      else{
        0.0
      }
    }
  }
  
  object docType extends LinkFeature("document Type"){
    def apply(link: KBPQueryLink) = {
      val docType = link.query.doc
      if(docType.startsWith("eng")){
        0.0
      }
      else{
        1.0
      }
    }
  }
  
  object corefContextLength extends LinkFeature("length of context"){
    def apply(link: KBPQueryLink) = {
      val context = link.query.corefSourceContext
      var numWords = 0.0
      for(line <- context){
        numWords += line.split(" ").length
      }
      numWords
    }
  }
  
  object fullNameLength extends LinkFeature("length of full name"){
    def apply(link: KBPQueryLink) = {
      link.query.entityString.split("[\\s,]+").length
    }
  }
  
  object originalNameLength extends LinkFeature("length of full name"){
    def apply(link: KBPQueryLink) = {
      link.query.name.split("[\\s,]+").length
    }
  }
  
  object namedEntityOverlap extends LinkFeature("Named Entity Overlap Score"){
    def apply(link: KBPQueryLink)= {
      val linkName = link.link.entity.name
      val kbId = link.query.helper.wikiMap.get(linkName).getOrElse("")
      var result = 0.0
      if(kbIdToNamedEntitiesMap.contains(kbId)){
        val namedEntities = kbIdToNamedEntitiesMap.get(kbId).get
        if(link.query.corefHelper.haveNamedEntityInCommon(link.query.baseDir, link.link.entity.name, link.query.id, targetNamedEntities = Some(namedEntities))){
          result = 1.0
        }
      }
      else{
	      if(link.query.corefHelper.haveNamedEntityInCommon(link.query.baseDir, link.link.entity.name, link.query.id)){
	        1.0
	      }
      }
      println("Returning value: " + result)
      result
    }
  }
  
  object namedEntityOverlapScore extends LinkFeature("NE score"){
    def apply(link: KBPQueryLink)= {
      val linkName = link.link.entity.name
      val kbId = link.query.helper.wikiMap.get(linkName).getOrElse("")
      var result = 0.0
      if(kbIdToNamedEntitiesMap.contains(kbId)){
        val namedEntities = kbIdToNamedEntitiesMap.get(kbId).get
        if(link.query.corefHelper.haveNamedEntityInCommon(link.query.baseDir, link.link.entity.name, link.query.id, targetNamedEntities = Some(namedEntities))){
          result = 1.0
        }
      }
      else{
	      if(link.query.corefHelper.haveNamedEntityInCommon(link.query.baseDir, link.link.entity.name, link.query.id)){
	        1.0
	      }
      }
      println("Returning value: " + result)
      result
    }
  }
  
  object nameAmbiguity extends LinkFeature("Ambiguity of Name in KB"){
    def apply(link: KBPQueryLink) = {
      val name = link.query.name
      val length = link.query.helper.kbIdToTitleMap.values.toList.filter(p => p.toLowerCase().contains(name.toLowerCase())).length
      println(link.query.id + "\t" + length)
      length.toDouble
    }
  }
  
  object nameLength extends LinkFeature("Length of name used in linker"){
    def apply(link: KBPQueryLink) = {
      link.query.entityString.split(" ").length
    }
  }

  private val features = Seq(candidateScore, docSimScore)

  
  def featureSet = new FeatureSet(SortedMap.empty[String, Feature[KBPQueryLink, Double]] ++ features.map(f => (f.name, f)).toMap) 
}