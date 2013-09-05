package edu.knowitall.tac2013.entitylinking.utils

object ConfidenceHelper {
  
  
  def getConfidence(threshold: Double, score: Double): Double = {
    if(score > threshold){
      val confidence = 80
      val remainingConfidence = 100 - confidence
      val confidenceStep = remainingConfidence / ((1.0 - threshold)*100)
      ((((score*100) - (threshold*100)).toInt * confidenceStep) + confidence)/100
    }
    else{
      .55
    }
  }

}