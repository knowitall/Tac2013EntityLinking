package edu.knowitall.tac2013.solr.query

import jp.sf.amateras.solr.scala.SolrClient
import edu.knowitall.collection.immutable.Interval
import scala.xml.XML
import java.util.Scanner
import scala.util.parsing.combinator.syntactical.TokenParsers


object SolrHelper {
  
  val client = new SolrClient("http://knowitall:knowit!@rv-n16.cs.washington.edu:9325/solr/oldCorpus")

  
  
  def getRawDoc(docId: String): String = {
    val query = client.query("docid:\""+ docId + "\"")
    val result = query.getResultAsMap()
    if(result.documents.length != 1){
      System.err.println(docId + " was not found in corpus");
      ""
    }
    else{
      result.documents.head("xml").toString
    }
  }
  
  
  def findSentence(docId: String, offset: Integer): String = {
    
	  val rawDoc = getRawDoc(docId)
	  
	  val lines = rawDoc.split("\n")
	  var charsRead = 0
	  var lastP = 0
	  var paragraphInterval :Option[Interval] = None
	  var index =0
	  while(paragraphInterval.isEmpty){
	    val line = lines(index)
	    if(line == "<P>"){
	      lastP = charsRead
	    }
	    
	    if(line == "<//P>" && charsRead > offset){
	      paragraphInterval = Some(Interval.closed(lastP,charsRead))
	    }
	    
	    index += 1
	    charsRead += line.size
	  }
	  
	  rawDoc.slice(paragraphInterval.get.start, paragraphInterval.get.end)
	  
  }

}