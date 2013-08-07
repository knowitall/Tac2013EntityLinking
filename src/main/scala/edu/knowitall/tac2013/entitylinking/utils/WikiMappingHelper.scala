package edu.knowitall.tac2013.entitylinking.utils

import scopt.OptionParser
import java.io.File
import java.io.PrintStream
import java.util.concurrent.atomic.AtomicInteger
import edu.knowitall.tool.sentence.OpenNlpSentencer
import edu.knowitall.common.Timing
import edu.knowitall.collection.immutable.Interval

/**
 * A tool for extracting from the TAC initial knowledge base, for each entity:
 * Node Id, ( a TAC-specific identifier, e.g. E0000001
 * Wiki Name ( wikipedia page name )
 */
object WikiMappingHelper {

  val entityCounter = new AtomicInteger(0)
  
  val entityRegex = "<entity wiki_title=\"([^\"]+)\" type=\"([^\"]+)\" id=\"([^\"]+)\" name=\"([^\"]+)\">".r
  
  val entityTextRegex = """id="([^"]+)"[\w\W]+?<wiki_text><!\[CDATA\[([\w\W]+?)\]\]></wiki_text>""".r
  
  val sentencer = new OpenNlpSentencer()
  
  def main(args: Array[String]) {
    var inputFile = "."
    var output = System.out
    
    val parser = new OptionParser() {
      arg("inputFile", "A file, or directory in which to look for files (recursively).", { s => inputFile = s })
      opt("outputFile", "Optional file for output, default stdout.", { s => output = new PrintStream(s) })
    }
    
    if (!parser.parse(args)) return
    else {
      val inputs = FileUtils.getFilesRecursive(new File(inputFile))
      val nsTime = Timing.time {
        run(inputs, output) 
      }
      System.err.println(s"Processed ${entityCounter.get} entities in ${Timing.Seconds.format(nsTime)}.")
    }
    
  }
  
  def run(files: Iterator[File], output: PrintStream){
    
    for(file <- files){
      val lines = scala.io.Source.fromFile(file)(scala.io.Codec.UTF8).getLines.toList.mkString("\n")
      val idTextTuples = for( entityTextRegex(id,text) <- entityTextRegex.findAllIn(lines)) yield  (id, getKBIntro(text) )
      for(t <- idTextTuples) output.println(t._1 + "\t" + t._2)
    }
    output.close()
  }
  
  
  def loadIdToIntroTextMap(lines: Iterator[String]): Map[String,String] = {
    System.err.println("Loading freebase id to text map...")
    val tabSplit = "\t".r
    lines.map { line =>
      tabSplit.split(line) match {
        case Array(id, text) => (id, text)
        case _ => throw new RuntimeException(s"Error parsing entity info: $line")
      }  
    } toMap
  }
  
  
  case class EntityInfo(val id: String, val name: String, val typ: String) {
    def serialize = Seq(id, name, typ).mkString("\t")
  }
  
  def processLine(line: String): Option[EntityInfo] = {
    line match {
      case entityRegex(title, typ, id, name) => Some(EntityInfo(id, name, typ))
      case _ => None
    }
  }
  
  def loadNameToNodeIdMap(lines: Iterator[String]): Map[String, String] = {
    System.err.println("Loading wikipedia name to node id map...")
    val tabSplit = "\t".r
    lines.map { line =>
      tabSplit.split(line) match {
        case Array(id, name, typ, _*) => (name, id)
        case _ => throw new RuntimeException(s"Error parsing entity info: $line")
      }  
    } toMap
    
  }
  
  def loadIdToTitleMap(lines: Iterator[String]): Map[String, String] = {
    System.err.println("Loading wikipedia name to node id map...")
    val tabSplit = "\t".r
    lines.map { line =>
      tabSplit.split(line) match {
        case Array(id, name, typ, _*) => (id, name)
        case _ => throw new RuntimeException(s"Error parsing entity info: $line")
      }  
    } toMap
    
  }
  
  def loadQueryToCorefMentionsMap(lines: Iterator[String]): Map[String,Seq[Interval]] = {
    System.err.println("Loading query to Coref Mentions map...")
    val tabSplit = "\t".r
    lines.map {line =>
      tabSplit.split(line) match{
        case Array(qId, e  @ _*) => {(qId,for(i <- e) yield{
          Interval.closed(i.split(",")(0).drop(1).toInt,i.split(",")(1).dropRight(1).trim().toInt)
        })}
        case _ => throw new RuntimeException(s"Error parsing query info: $line")
        }
      } toMap
  }
  
  def getKBIntro(text: String) :String = {
    try{
      sentencer.segmentTexts(text)(0).replaceAll("\\s+", " ")
    }
    catch{
      case e: Exception => {
        text.take(100).replaceAll("\\s+", " ")
      }
    }
  }  
}