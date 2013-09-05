package edu.knowitall.tac2013.entitylinking

import scala.xml.XML
import edu.knowitall.common.Resource.using
import edu.knowitall.tac2013.entitylinking.utils.WikiMappingHelper
import edu.knowitall.tac2013.entitylinking.utils.FileUtils
import java.io.File
import edu.knowitall.tac2013.entitylinking.utils.StanfordAnnotatorHelperMethods
import edu.knowitall.collection.immutable.Interval
import edu.knowitall.tac2013.entitylinking.coref.CorefHelperMethods

class KBPQuery(val id: String, val name: String, val doc: String,
  val begOffset: Int, val endOffset: Int, val baseDir: String, val year: String) {

  var entityString = name
  var sportsSense: Option[Boolean] = None
  var highestLinkClassifierScore = 0.0

  val helper = KBPQuery.getHelper(baseDir, year)
  val corefHelper = CorefHelperMethods.get(year)

  private def getSourceContext(): String = {
    SolrHelper.getContextFromDocument(doc, begOffset, name,year)
  }

  private def getWideContext(): String = {
    SolrHelper.getWideContextFromDocument(doc, begOffset, name, year)
  }
  
  private def getHeadLineContext(): List[String] = {
    SolrHelper.getHeadLineContextFromDocument(doc, year)
  }

  private def getContextOfAllMentions(): List[String] = {
    var contextualSentences = List[String]()
    val corefMentions = corefHelper.queryToCorefMap(id)
    for (cmi <- corefMentions) {
      val contextSentence = SolrHelper.getContextFromDocument(doc, cmi.start, name, year)
      contextualSentences = contextualSentences :+ contextSentence
    }
    var totalContext = List[String]()
    val corefContext = ((contextualSentences.toList ::: List(getSourceContext())).toSet).toList
    totalContext = corefContext
    val headLineContext = getHeadLineContext()
    for(hlc <- headLineContext){
      var addToContext = true
      for(cc <- corefContext){
        if(cc.contains(hlc)){
          addToContext = false
        }
      }
      if(addToContext){
          totalContext = hlc :: totalContext        
      }
    }
    totalContext.toList
  }

  val sourceContext = getSourceContext()
  val sourceWideContext = getWideContext()

  val corefSourceContext = getContextOfAllMentions()
  val stanfordNERType = CorefHelperMethods.get(year).getStanfordNERType(id, year)

  def trimSourceContext(): String = {
    val stringOffsetOfEntity = sourceContext.indexOf(name)

    stringOffsetOfEntity match {
      case -1 => sourceContext.slice((sourceContext.length() / 2) - 40, ((sourceContext.length() / 2) + 40))
      case _ => sourceContext.slice(stringOffsetOfEntity - 40, stringOffsetOfEntity + 40 + name.length())
    }

  }

  //debug output on construction
  //System.err.println("KBPQuery for entity: " + name +" has context sentence of: " + sourceContext)
}

object KBPQuery {

  private val helperCache = new scala.collection.mutable.HashMap[(String, String), KBPQueryHelper]

  def getHelper(baseDir: String, year: String, fromScratch: Boolean = false) = helperCache.getOrElseUpdate((baseDir, year), KBPQueryHelper(baseDir, year, fromScratch))
}

case class KBPQueryHelper(val baseDir: String, val year: String, val fromScratch: Boolean) {

  
  val corefHelper = new StanfordAnnotatorHelperMethods(fromScratch)

  private def parseSingleKBPQueryFromXML(queryXML: scala.xml.Node): Option[KBPQuery] = {

    //val pathToXML = Source.fromFile(pathToFile)
    try {
      val idText = queryXML.attribute("id") match {
        case Some(id) if id.length == 1 => id(0).text
        case None => throw new IllegalArgumentException("no id value for query in xml doc")
      }
      val nameText = queryXML.\\("name").text
      val docIDText = queryXML.\\("docid").text
      val begText = queryXML.\\("beg").text
      val begInt = begText.toInt
      val endText = queryXML.\\("end").text
      val endInt = endText.toInt

      val x = new KBPQuery(idText, nameText, docIDText, begInt, endInt, baseDir, year)
      Some(x)
    } catch {
      case e: Exception => {
        parseSingle2011KBPQueryFromXML(queryXML)
      }
    }
  }

  private def parseSingle2011KBPQueryFromXML(queryXML: scala.xml.Node): Option[KBPQuery] = {

    try {
      val idText = queryXML.attribute("id") match {
        case Some(id) if id.length == 1 => id(0).text
        case None => throw new IllegalArgumentException("no id value for query in xml doc")
      }
      val nameText = queryXML.\\("name").text
      val docIDText = queryXML.\\("docid").text

      val x = new KBPQuery(idText, nameText, docIDText, -1, -1, baseDir, year)
      Some(x)
    } catch {
      case e: Exception => {
        println("returned NONE!")
        None
      }
    }
  }

  def parseKBPQueries(path: String): List[KBPQuery] = {

    val xml = XML.loadFile(path)
    val queryXMLSeq = xml.\("query")
    val kbpQueryList = for (qXML <- queryXMLSeq) yield parseSingleKBPQueryFromXML(qXML)

    kbpQueryList.toList.flatten
  }

  val mapFile = baseDir + "/wikimap.txt"
  val wikiMap = using(io.Source.fromFile(mapFile, "UTF8")) { source =>
    WikiMappingHelper.loadNameToNodeIdMap(source.getLines)
  }
  
  val kbContextMapFile = baseDir + "/kbIdToFirstParagraph.txt"
  //val kbContextMap = using(io.Source.fromFile(kbContextMapFile, "UTF8")) { source =>
   // WikiMappingHelper.loadKbIdToContextMap(source.getLines)
 // }

  val kbIdToWikiStructuredTypeFile = getClass.getResource("kbIdToWikiStructuredTypeMap.txt").getPath()
  val kbIdToWikiStructuredTypeMap = using(io.Source.fromFile(kbIdToWikiStructuredTypeFile, "UTF8")) { source =>
    WikiMappingHelper.loadKbIdTowikiStructuredTypeMap(source.getLines)
  }

  val kbIdTextToMapFile = baseDir + "/kbIdToTextMap.txt"
  val kbIdTextMap = using(io.Source.fromFile(kbIdTextToMapFile, "UTF8")) { source =>
    WikiMappingHelper.loadIdToIntroTextMap(source.getLines)
  }
  val kbToTitleMapFile = baseDir + "/wikimap.txt"
  val kbIdToTitleMap = using(io.Source.fromFile(kbToTitleMapFile, "UTF8")) { source =>
    WikiMappingHelper.loadIdToTitleMap(source.getLines)
  }

  val kbTitleToIdMapFile = getClass.getResource("kbIdToTitleMap.txt").getPath()
  val kbTitleToIdMap = using(io.Source.fromFile(kbTitleToIdMapFile, "UTF8")) { source =>
    WikiMappingHelper.loadKbTitleToIdMap(source.getLines)
  }
  val kbIdToWikiTypeFile = getClass.getResource("kbIdToWikiTypeMap.txt").getPath()
  val kbIdToWikiTypeMap = using(io.Source.fromFile(kbIdToWikiTypeFile, "UTF8")) { source =>
    WikiMappingHelper.loadKbIdToWikiTypeMap(source.getLines)
  }
}