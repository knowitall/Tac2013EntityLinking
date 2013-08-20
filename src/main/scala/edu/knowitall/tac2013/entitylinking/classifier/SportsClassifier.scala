package edu.knowitall.tac2013.entitylinking.classifier

import edu.knowitall.tool.conf.BreezeLogisticRegressionTrainer
import edu.knowitall.tool.conf.ConfidenceTrainer
import edu.knowitall.tool.conf.ConfidenceFunction
import edu.knowitall.tool.conf.Labelled
import edu.knowitall.tool.conf.FeatureSet
import edu.knowitall.browser.entity.EntityLink
import edu.knowitall.tac2013.entitylinking.classifier.SportsSenseTrainingData.SportsSenseInstance
import scala.util.Random

class SportsClassifier(val trainingData: Iterable[Labelled[SportsSenseInstance]]) {
  
    val trainer = new BreezeLogisticRegressionTrainer(SportsSenseFeatures.featureSet)
  
    val classifier = trainer.train(trainingData)
  
    def score(ssi: SportsSenseInstance): Double = classifier(ssi) 
}

object SportsClassifier{
  
  
  def main(args: Array[String]){
    val baseDir = args(0)
    val allData = Random.shuffle(SportsSenseTrainingData.getTrainingData(baseDir, "2011").toList ::: (SportsSenseTrainingData.getTrainingData(baseDir, "2012").toList))
    val allDataSize = allData.size
    val testDataSize = math.ceil(allDataSize * .2).toInt
    val trainingData = allData.drop(testDataSize).toIterable
    val testData = allData.take(testDataSize).toIterable
    val naiveBayesModelData = allData.take(math.ceil(allDataSize * .5).toInt).toIterable
    
    
    val nbModel = SportsSenseTrainingData.getNBModel(naiveBayesModelData)
    for(trainingInstance <- trainingData ){
      trainingInstance.item.naiveBayesScore = Some(nbModel.scores(
          SportsSenseTrainingData.getContextCounter(trainingInstance.item.kbpQuery.sourceWideContext))
          (true))
    }
    for(testInstance <- testData ){
      testInstance.item.naiveBayesScore = Some(nbModel.scores(
          SportsSenseTrainingData.getContextCounter(testInstance.item.kbpQuery.sourceWideContext))
          (true))
    }
    
    val classifier = new SportsClassifier(trainingData)
    
    var classifyOutput = List[(String,Boolean,Double,Double)]()
    for(testInstance <- testData){
      val trueLabel = testInstance.label
      val score = classifier.score(testInstance.item)
      val confidence =  classifier.classifier.getConf(testInstance.item)
      classifyOutput = (testInstance.item.kbpQuery.name,trueLabel,score,confidence) :: classifyOutput
      //println(testInstance.item.kbpQuery.name +"\t" + trueLabel +"\t" + score + "\t" + confidence)
    }
    for( inst <- classifyOutput.toList.toSeq.sortBy(f => f._3)){
      println(inst._1 +"\t" + inst._2 +"\t" + inst._3 + "\t" + inst._4)
    }
    
  }
  
}
