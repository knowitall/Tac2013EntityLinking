package edu.knowitall.tac2013.entitylinking.classifier

import java.io.PrintWriter
import java.io.File
import edu.knowitall.tac2013.entitylinking.KBPQuery
import scala.util.control.Breaks._
import edu.knowitall.common.Resource.using

object LinkFeatureWriter {
  
  
  def main(args: Array[String]) = {
    //val lines2012 = scala.io.Source.fromFile(getClass().getResource("linker-classifier-training-2012.csv").getPath())(scala.io.Codec.UTF8).getLines
    //val lines2011 = scala.io.Source.fromFile(getClass().getResource("linker-classifier-training-2011.csv").getPath())(scala.io.Codec.UTF8).getLines
    
    //writeQueryToAmbiguityFile(lines2012,"2012")
    //writeQueryToAmbiguityFile(lines2011,"2011")
    
    writeKbIdToNamedEntitiesFile()
  }
  
  def writeQueryToAmbiguityFile(lines: Iterator[String], year: String) {
    val queries = KBPQuery.getHelper("/scratch", year).parseKBPQueries(getClass.getResource("tac_"+ year +"_kbp_english_evaluation_entity_linking_queries.xml").getPath())
    val qMap = queries.map(q => (q.id, q)).toMap

    val pw = new PrintWriter(new File("linker-classifier-training-"+year+".csv"))

      for(line <- lines){
        val qId = line.split("\t") match {
          case Array(label, qid, used, expected, _*) => {
            qid
          }
          case _ => { ""}
        }
        if(qMap.contains(qId)){
          val q =  qMap.get(qId).get
          val name = q.name
          val length = q.helper.kbIdToTitleMap.values.toList.filter(p => p.contains(name)).length
          pw.write(line+"\t"+length)
        }
      }
      pw.close
  }
  
  def writeKbIdToNamedEntitiesFile(){
     var baseDir = "/scratch2/"
     val lines2012 = scala.io.Source.fromFile(getClass().getResource("linker-classifier-training-2012.csv").getPath())(scala.io.Codec.UTF8).getLines
     val lines2011 = scala.io.Source.fromFile(getClass().getResource("linker-classifier-training-2011.csv").getPath())(scala.io.Codec.UTF8).getLines
     val lines = lines2011.toList ::: lines2012.toList
     val pw = new PrintWriter(new File("KbNamedEntitiesMap.txt"))
     
     var writtenKbIds = List[String]()
     for(line <- lines){
       val kbName = line.split("\t") match {
          case Array(label, qid, used, expected, _*) => {
            expected
          }
          case _ => { ""}
        }
       val kbId = KBPQuery.getHelper(baseDir, "2012").wikiMap.get(kbName).getOrElse("")
       val kbContextMapFile = KBPQuery.getHelper(baseDir, "2012").kbContextMapFile

       var kbContext = ""
       		breakable{
			    using(io.Source.fromFile(kbContextMapFile, "UTF8")) { source =>
			      val lines = source.getLines
			      val tabSplit = """\t""".r
			      lines.foreach(f => {
			        if(tabSplit.split(f)(0).trim() == kbId){
			          try{
			            kbContext = tabSplit.split(f)(1).trim()
			          }
			          catch{
			            case e: Exception => {
			              kbContext = " "
			            }
			          }
			          break
			        }
			      })
			     }
		    }
       if(kbContext != " " && !writtenKbIds.contains(kbId)){
         val namedEntities = scala.collection.JavaConversions.asScalaIterable(KBPQuery.getHelper(baseDir, "2012").corefHelper.getNamedEntities(kbContext)).toList
         var neString = ""
         for(namedEntity <- namedEntities){
           neString += "\t" +namedEntity
         }
         pw.write(kbId + neString +"\n")
         writtenKbIds = kbId :: writtenKbIds
       }
     }
     pw.close()
    
  }

}