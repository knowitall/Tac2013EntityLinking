package edu.knowitall.tac2013.solr.query

import jp.sf.amateras.solr.scala.SolrClient
import edu.knowitall.collection.immutable.Interval
import scala.xml.XML
import java.util.Scanner
import scala.util.parsing.combinator.syntactical.TokenParsers
import scala.util.matching.Regex
import edu.knowitall.tool.sentence.OpenNlpSentencer


object SolrHelper {
  
  val client = new SolrClient("http://knowitall:knowit!@rv-n16.cs.washington.edu:9325/solr/oldCorpus")
  val xmlTagPair = new Regex("<[^<]+>[^<]*</[^<]+>")
  val xmlTagPattern = new Regex("</?[^<]+>")
  val sentencer = new OpenNlpSentencer()

  
  /**
   * Return a raw doc from the old corpus
   */
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
  
  /***
   * Find a sentence from the Document if it can be found by using the XML <P> </P> format
   */
  def getSentenceFromDocWithParagraphFormat(docId:String, offset:Integer, name:String) :Option[String] = {
    
      try{
		  val rawDoc = getRawDoc(docId)
		  val lines = rawDoc.split("\n")
		  
		  var charsRead = 0
		  var lastP = 0
		  var paragraphInterval :Option[Interval] = None
		  var index =0
		  
		  //iterate over <P> until the Paragraph envelops the offset of the entity
		  while(paragraphInterval.isEmpty){
		    val line = lines(index)
		    if(line == "<P>"){
		      lastP = charsRead
		    }
		    
		    if(line == "</P>" && charsRead > offset){
		      paragraphInterval = Some(Interval.closed(lastP,charsRead))
		    }
		    
		    index += 1
		    charsRead += (line.size + 1)
		  }
		  
		  //if the paragraph is not found return None
		  if(paragraphInterval.isEmpty){
		    return None
		  }
		  
		  //if the paragraph is found use the OpenNlpSentencer
		  else{
		    val paragraph = rawDoc.slice(paragraphInterval.get.start, paragraphInterval.get.end)
		    //remove remaining xml from paragraph
	        val paragraphMinusAllXML = xmlTagPattern.replaceAllIn(paragraph,"")
	        
	        //use the OpenNlpSentence to find the first sentence that contains
	        //the entity name
	        val sentences = sentencer.segmentTexts(paragraphMinusAllXML)
		    val entitySentence = sentences.filter(p => p.contains(name)).headOption
		    if(entitySentence.isDefined){
		      return Some(entitySentence.get.trim())
		    }
		    //if no appropriate sentence found return None
		    else{
		    return None
		    }
		  }
      }
      catch {
        case e: Exception => {
          None
        }
      }
  }
  
  /**
   * Take up to 100 characters, take off first and last char sequences and 
   * turn the chars into a single line string
   */
  def getCharacterContext(docId:String, offset:Integer): String = {
    val charContext = getRawDoc(docId).slice(offset-50, offset+50)
    
    val charContextArray = charContext.split("\\s")
    val tokenizedContext = charContextArray.slice(1, charContextArray.length-1).mkString(" ")
	xmlTagPattern.replaceAllIn(tokenizedContext,"")
    
  }
  
  /**
   * Try finding a sentence with the OpenNlpSentence without the helpful
   * <P> and </P> XML tags
   */
  def getUnformattedSentenceFromDoc(docId:String, offset:Integer, name:String) : Option[String] = {
    val rawDoc = getRawDoc(docId)
    val leftcharSeq = rawDoc.slice(0, offset)
    val rightcharSeq = rawDoc.slice(offset,rawDoc.size)
    val afterXML = leftcharSeq.reverse.takeWhile(p => {p != '>'}).reverse
    val beforeXML = rightcharSeq.takeWhile(p => {p != '<'})
    val text = afterXML + beforeXML
    val sentences = sentencer.segmentTexts(text)
    for(s <- sentences){
      if(s.contains(name))
        return Some(s)
    }
    None
  }
  
  /***
   * Return the a string from the document giving the context based on the KBP 
   * Query offsets. If a sentence can reasonably be found return it, else return
   * a section of characters
   */
  def getContextFromDocument(docId: String, offset: Integer, name: String): String = {
    //initlaize context variable
    var context = ""
      
    val sentence = getSentenceFromDocWithParagraphFormat(docId:String, offset:Integer, name: String)
    
    //if sentence was not found from <P> </P> format fall back onto less robust
    //sentence finders and character context finder
    if(sentence.isEmpty){
      //try using the sentence on docs without <P> format
      val unformattedSentence = getUnformattedSentenceFromDoc(docId,offset,name)
      if(unformattedSentence.isEmpty)
        //if no sentence was found just get the character context
        context = getCharacterContext(docId:String, offset:Integer)
      else
        context = unformattedSentence.get
    }
    else
      context = sentence.get
      
    //make context a single line
    context.replaceAll("\\s+", " ")
  }

}