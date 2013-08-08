package edu.knowitall.tac2013.entitylinking.utils

import edu.knowitall.tac2013.entitylinking.FormattedOutput
import edu.knowitall.tac2013.entitylinking.KBPQuery
import edu.knowitall.common.Resource.using
import scopt.OptionParser
import scala.io.Source

//Constructor takes formatted output and turns it into human readable output
//Output should be QueryId \t entityString \t sourceSentence \t linkId \t FirstSentenceFromKB
class FormattedOutputToHumanReadableOutputConverter(formattedOutput: FormattedOutput, kbpQuery: KBPQuery) {
  
  val queryId = formattedOutput.queryId
  val linkId = formattedOutput.kbLink
  val confidence = formattedOutput.confidence
  val entityString = kbpQuery.name
  val sourceContext = kbpQuery.corefSourceContext
  val kbSentence = KBPQuery.kbIdTextMap.getOrElse({throw new Exception("Did not active KBP Query")}).get(linkId).getOrElse({"None"})
  val docId = kbpQuery.doc
  val kbTitle = KBPQuery.kbIdToTitleMap.getOrElse({throw new Exception("Did not activate KBP Query")}).get(linkId).getOrElse({"None"})
  
  override def toString(): String = {
    Iterator(queryId,entityString,docId,sourceContext,linkId,kbTitle,kbSentence).mkString("\t")
  }

}


object FormattedOutputToHumanReadableOutputConverter{
  
  var queryFile = ""
  var outputFile = ""
  var newOutputFile = ""
  var baseDir = ""
  
  /**
   * Used to read an output file and a set of queries
   * and put them in a human readable format
   */
  def main(args: Array[String]){
    
    val argParser = new OptionParser() {
      arg("queryFile", "Path to KBP Query File .", { s => queryFile = s })
      arg("outputFile", "Path to the KBP formatted outputfile", {s => outputFile = s})
      arg("newOutputFile", "Path to the new human readable output file", {s => newOutputFile = s})
      arg("baseDir", "Path to the base directory with large files", {s => baseDir= s})
    }

    if(!argParser.parse(args)) return
    
    //activate maps in KBPQuery
    KBPQuery.activate(baseDir)
    
    val queries = KBPQuery.parseKBPQueries(queryFile)
    val formattedOutputLines = Source.fromFile(outputFile)(scala.io.Codec.UTF8).getLines.toList
    
    
    if(queries.size != formattedOutputLines.length){
      throw new Exception("Query and formattedOutput Files do not match up in length")
    }
    
    val formattedOutputList = formattedOutputLines.zip(queries)
    var humanReadableLines = List[FormattedOutputToHumanReadableOutputConverter]()
    
    for( formattedOutputEntry <- formattedOutputList){
      val formattedLine = formattedOutputEntry._1
      val query = formattedOutputEntry._2
      humanReadableLines = humanReadableLines :+ new FormattedOutputToHumanReadableOutputConverter(FormattedOutput.readFormattedOutput(formattedLine),query)
    }
    
    val pw = new java.io.PrintWriter(new java.io.File(newOutputFile))
    for(humanReadableLine <- humanReadableLines){
      pw.write(humanReadableLine.toString())
    }
    
    pw.close()
    
  }  
}