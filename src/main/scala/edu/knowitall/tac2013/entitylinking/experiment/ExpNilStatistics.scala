package edu.knowitall.tac2013.entitylinking.experiment

import java.io.File
import edu.knowitall.tac2013.entitylinking.coref.CorefHelperMethods

object ExpNilStatistics {
  
  def main(args: Array[String]) = {
    
    val benchmarkFile = new File(args(0))
    val lines = scala.io.Source.fromFile(benchmarkFile)(scala.io.Codec.UTF8).getLines
    
    var totalExpNil = 0.0
    var shareNamedEntity =0.0
    var totalCorrect =0.0
    var correctShareNamedEntity = 0.0
    var totalWRONGKB =0.0
    var numberShareNamedEntityWrongKB = 0.0
    for(line <- lines){
      val lineValues = line.split("\t")
      if(lineValues.length > 9){
	      val linkName = lineValues(9).trim()
	      val queryName = lineValues(4).trim()
	      val fullName  = lineValues(5).trim()
	      val queryId = lineValues(3).trim()
	      if(fullName != linkName && queryName != linkName){
		      if(lineValues(0).startsWith("EXP NIL")){
		        if(CorefHelperMethods.get("2012").haveNamedEntityInCommon("/scratch/resources/entitylinkingResources", linkName, queryId)){
		          shareNamedEntity += 1
		        }
		        totalExpNil += 1
		      }
		      if(lineValues(0).startsWith("CORRECT")){
		        if(CorefHelperMethods.get("2012").haveNamedEntityInCommon("/scratch/resources/entitylinkingResources", linkName, queryId)){
		          correctShareNamedEntity += 1
		        }
		        totalCorrect +=1 
		      }
		      if(lineValues(0).startsWith("WRONG KB")){
		        if(CorefHelperMethods.get("2012").haveNamedEntityInCommon("/scratch/resources/entitylinkingResources", linkName, queryId)){
		          numberShareNamedEntityWrongKB += 1
		        }
		        totalWRONGKB +=1 		        
		      }
	      }
      }
    }
    

    println("Number of Correct: " + totalCorrect)
    println("Number of Correct that Share Named Entity: " + correctShareNamedEntity)
    println("Percentage : " + correctShareNamedEntity/totalCorrect)
    println("Number of Exp NIL: " + totalExpNil)
    println("NUmber of Exp Nil that share named entity: " + shareNamedEntity)
    println("Percentage: " + shareNamedEntity/totalExpNil)
    println("Number of Wrong KB: " + totalWRONGKB)
    println("NUmber of Wrong KB that share named entity: " + numberShareNamedEntityWrongKB)
    println("Percentage: " + numberShareNamedEntityWrongKB/totalWRONGKB)
  }

}