package edu.knowitall.tac2013.entitylinking.utils

import scala.xml.XML

object XMLHelper {
  
  def parseKBPQueryXML(path: String, year: String) : List[QuerySpecification] = {
    
      val xml = XML.loadFile(path)
      val queryXMLSeq = xml.\("query")
      val kbpQueryList = for (qXML <- queryXMLSeq) yield{
          val queryXML = qXML
	      val idText = queryXML.attribute("id") match {
	        case Some(id) if id.length == 1 => id(0).text
	        case None => throw new IllegalArgumentException("no id value for query in xml doc")
	      }
	      val nameText = queryXML.\\("name").text
	      val docIDText = queryXML.\\("docid").text
	      try{
		      val begText = queryXML.\\("beg").text
		      val begInt = begText.toInt
		      val endText = queryXML.\\("end").text
		      val endInt = endText.toInt
		      new QuerySpecification(idText,nameText,docIDText,year,begInt,endInt)
	      }
	      
	      catch{
	        case e: Exception => {
	          new QuerySpecification(idText,nameText,docIDText,year,-1,-1)
	        }
	      }
      }
    
      kbpQueryList.toList
  }
  
  
  
  
  
  
  
  case class QuerySpecification(val id: String, val name: String, val doc: String, val year: String, val begOffSet: Int, val endOffset: Int)

}