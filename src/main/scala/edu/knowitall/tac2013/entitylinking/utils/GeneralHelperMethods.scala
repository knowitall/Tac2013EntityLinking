package edu.knowitall.tac2013.entitylinking.utils

object GeneralHelperMethods {

  def main(args: Array[String]) {
    
    val backOffs = findBackOffStrings("Ford", "UTC Graham Ford")
    for (bo <- backOffs){
      println(bo)
    }
  }
  
  def findBackOffStrings(queryName: String, fullString: String): List[String] = {
    val originalLength = queryName.split(" ").length
    val fullLength = fullString.split(" ").length
    var backOffStrings = List[String]()
    var words = fullString.split(" ")
    var startIndex = 0
    while((fullLength - startIndex) > originalLength){
      var stopIndex = fullLength
      while((stopIndex - startIndex) > originalLength){
        backOffStrings = fullString.split(" ").slice(startIndex, stopIndex).mkString(" ") :: backOffStrings
        stopIndex -= 1
      }
      startIndex += 1
    }
    val backOffStringList = backOffStrings.filter(p => p.contains(queryName)).toList
    backOffStringList
  }
}