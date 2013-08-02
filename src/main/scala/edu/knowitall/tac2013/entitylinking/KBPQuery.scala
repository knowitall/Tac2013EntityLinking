package edu.knowitall.tac2013.entitylinking

import scala.xml.XML

class KBPQuery (val id: String, val name: String, val doc: String,
    val begOffset: Int, val endOffset: Int){
  
  private def getSourceSentence(): String = {
    this.name
  }
  
  val sourceSentence = getSourceSentence()
}

object KBPQuery{
  
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
	    
	    Some(new KBPQuery(idText,nameText,docIDText,begInt,endInt))
    }
    catch {
      case e: Exception => {
        println(e.getMessage())
        return None
        
      }
    }
  }
  
  def parseKBPQueries(): List[KBPQuery] = {
    
    val xml = XML.loadFile(getClass.getResource("tac_2012_kbp_english_evaluation_entity_linking_queries.xml").getPath())
    val queryXMLSeq = xml.\("query")
     
     val kbpQueryList = for( qXML <- queryXMLSeq) yield parseSingleKBPQueryFromXML(qXML)
    
     kbpQueryList.toList.flatten
  }
}