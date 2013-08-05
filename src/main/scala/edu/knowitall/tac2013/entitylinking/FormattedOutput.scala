package edu.knowitall.tac2013.entitylinking

class FormattedOutput(val queryId: String, val kbLink: String, val confidence: Double) {

}

object FormattedOutput{
  
 
  def readFormattedOutput(line: String): FormattedOutput = {
    val vals = line.split("\t")
    val queryId = vals(0)
    val kbLink = vals(1)
    var confidence = 0.0
    new FormattedOutput(queryId,kbLink,confidence)
  }
  
  
  
  
}