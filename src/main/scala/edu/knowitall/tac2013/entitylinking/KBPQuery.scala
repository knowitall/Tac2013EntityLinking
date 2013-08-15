package edu.knowitall.tac2013.entitylinking

import scala.xml.XML
import edu.knowitall.common.Resource.using
import edu.knowitall.tac2013.entitylinking.utils.WikiMappingHelper
import edu.knowitall.tac2013.entitylinking.utils.FileUtils
import java.io.File
import edu.knowitall.tac2013.entitylinking.utils.StanfordAnnotatorHelperMethods
import edu.knowitall.collection.immutable.Interval

class KBPQuery (val id: String, val name: String, val doc: String,
    val begOffset: Int, val endOffset: Int){
  
  var entityString = name
  
  private def getSourceContext(): String = {
    SolrHelper.getContextFromDocument(doc, begOffset, name)
  }
  
  private def getWideContext(): String = {
    SolrHelper.getWideContextFromDocument(doc,begOffset,name)
  }
  
  private def getContextOfAllMentions(): List[String] = {
    var contextualSentences = List[String]()
    val corefMentions = KBPQuery.queryToCorefMentionsMap.get(id)
    for(cmi <- corefMentions){
      val contextSentence = SolrHelper.getContextFromDocument(doc,cmi.start,name)
      contextualSentences = contextualSentences :+ contextSentence
    }
    ((contextualSentences.toList ::: List(getSourceContext())).toSet).toList
  }
  
  val sourceContext = getSourceContext()
  val sourceWideContext = getWideContext()
  lazy val corefSourceContext = getContextOfAllMentions()
  
  
  def trimSourceContext():String = {
    val stringOffsetOfEntity = sourceContext.indexOf(name)
    
    stringOffsetOfEntity match{
      case -1 => sourceContext.slice((sourceContext.length()/2)-40, ((sourceContext.length()/2)+40))
      case _ => sourceContext.slice(stringOffsetOfEntity-40, stringOffsetOfEntity + 40 + name.length())
    }
    
  }
  
  
  //debug output on construction
  //System.err.println("KBPQuery for entity: " + name +" has context sentence of: " + sourceContext)
}

object KBPQuery{
  
  var wikiMap :Option[Map[String,String]] = None
  var kbIdToTitleMap :Option[Map[String,String]] = None
  var kbIdTextMap :Option[Map[String,String]] = None
  var queryToCorefMentionsMap : Option[Map[String,Seq[Interval]]] = None
  var kbTitleToIdMap :Option[Map[String,String]] = None
  var year :Option[String] = None
  
  val corefHelper = new StanfordAnnotatorHelperMethods(true)
  
  private def parseSingleKBPQueryFromXML(queryXML: scala.xml.Node): Option[KBPQuery] = {
    

    //val pathToXML = Source.fromFile(pathToFile)
    try{
	    val idText = queryXML.attribute("id") match 
	    		{case Some(id) if id.length ==1 => id(0).text
	    		 case None => throw new IllegalArgumentException("no id value for query in xml doc")
	    		}
	    val nameText = queryXML.\\("name").text
	    val docIDText = queryXML.\\("docid").text
	    val begText = queryXML.\\("beg").text
	    val begInt = begText.toInt
	    val endText = queryXML.\\("end").text
	    val endInt = endText.toInt
	    
	    val x = new KBPQuery(idText,nameText,docIDText,begInt,endInt)
	    Some(x)
    }
    catch {
      case e: Exception => {
        parseSingle2011KBPQueryFromXML(queryXML)
      }
    }
  }
  
  private def parseSingle2011KBPQueryFromXML(queryXML: scala.xml.Node): Option[KBPQuery] = {
    //val pathToXML = Source.fromFile(pathToFile)
    try{
	    val idText = queryXML.attribute("id") match 
	    		{case Some(id) if id.length ==1 => id(0).text
	    		 case None => throw new IllegalArgumentException("no id value for query in xml doc")
	    		}
	    val nameText = queryXML.\\("name").text
	    val docIDText = queryXML.\\("docid").text

	    
	    val x = new KBPQuery(idText,nameText,docIDText,-1,-1)
	    Some(x)
    }
    catch {
      case e: Exception => {
        println("returned NONE!")
    	 None
      }
    }
  }
  
  def parseKBPQueries(path: String): List[KBPQuery] = {
    
    val xml = XML.loadFile(path)
    val queryXMLSeq = xml.\("query")
    val kbpQueryList = for( qXML <- queryXMLSeq) yield parseSingleKBPQueryFromXML(qXML)
    
     kbpQueryList.toList.flatten
  }
  
  def activate (baseDir: String, year:String) {
      this.year = Some(year)
	  val mapFile = baseDir + "/wikimap.txt"
	  wikiMap = using(io.Source.fromFile(mapFile, "UTF8")) { source =>
	      Some(WikiMappingHelper.loadNameToNodeIdMap(source.getLines))
	    }
	  val kbIdTextToMapFile = baseDir + "/kbIdToTextMap.txt"
	  kbIdTextMap = using(io.Source.fromFile(kbIdTextToMapFile, "UTF8")) { source =>
	      Some(WikiMappingHelper.loadIdToIntroTextMap(source.getLines))
	    }
	  val kbToTitleMapFile = baseDir + "/wikimap.txt"
	  kbIdToTitleMap = using(io.Source.fromFile(kbToTitleMapFile,"UTF8")) { source =>
	    Some(WikiMappingHelper.loadIdToTitleMap(source.getLines))
	    }
	  val corefMentionsFile = getClass.getResource("/edu/knowitall/tac2013/entitylinking/coref/"+year+"corefmentions.txt").getPath()
	  queryToCorefMentionsMap = using(io.Source.fromFile(corefMentionsFile,"UTF8")) { source =>
	    Some(WikiMappingHelper.loadQueryToCorefMentionsMap(source.getLines))}
	  val kbTitleToIdMapFile = getClass.getResource("kbIdToTitleMap.txt").getPath()
	  kbTitleToIdMap = using(io.Source.fromFile(kbTitleToIdMapFile,"UTF8")) { source =>
	    Some(WikiMappingHelper.loadKbTitleToIdMap(source.getLines))}
  }
}