package edu.knowitall.tac2013.entitylinking.classifier

import edu.knowitall.tool.conf.BreezeLogisticRegressionTrainer
import edu.knowitall.tool.conf.ConfidenceTrainer
import edu.knowitall.tool.conf.ConfidenceFunction
import edu.knowitall.tool.conf.Labelled
import edu.knowitall.tool.conf.FeatureSet
import edu.knowitall.browser.entity.EntityLink
import edu.knowitall.tac2013.entitylinking.classifier.SportsSenseTrainingData.SportsSenseInstance
import scala.util.Random
import edu.knowitall.tac2013.entitylinking.SolrHelper
import java.io.PrintWriter
import java.io.File
import java.io.Writer
import java.io.FileOutputStream
import breeze.classify.NaiveBayes
import java.io.FileInputStream
import org.apache.commons.io.IOUtils
import org.apache.commons.io.FileUtils
import scopt.OptionParser
import edu.knowitall.tac2013.entitylinking.KBPQuery

class SportsClassifier(val trainingData: Iterable[Labelled[SportsSenseInstance]]) {
  
    val trainer = new BreezeLogisticRegressionTrainer(SportsSenseFeatures.featureSet)
  
    val classifier = trainer.train(trainingData)
    
    def score(ssi: SportsSenseInstance): Double = classifier(ssi) 
}

object SportsClassifier{
  
  
  def main(args: Array[String]){
    
    
      var methodChoice = -1
      var baseDir = "/scratch"
      var year = ""
      
      val parser = new OptionParser("Sports Classifier") {
      arg("method", "0 or 1, 0 for test, 1 for writing", {i => methodChoice = i.toInt} )
      opt("year", "Year of queries to run on", {s => year =s })
      opt("basedir", "basedir", { s => baseDir = s })
    }
      
      if(!parser.parse(args)) return
      
      println(methodChoice)
      methodChoice match{
        case 0 => {testClassifier(baseDir)}
        case 1 => {writeClassifier(baseDir)}
        case _ => {throw new Exception("Invalid method choice. Choose 0 or 1")}
      }
    
  }
  
  
  def testClassifier(baseDir: String) {
    val trainingDataFile = new File("sportsTrainingData.out")
    var allData = List[Labelled[SportsSenseInstance]] ()
    if(!trainingDataFile.exists()){
      allData = Random.shuffle(SportsSenseTrainingData.getTrainingData(baseDir, "2011").toList ::: (SportsSenseTrainingData.getTrainingData(baseDir, "2012").toList))
      val pw = new FileOutputStream(trainingDataFile)
      pw.write(scala.util.Marshal.dump(allData))
    }
    else{
      allData = scala.util.Marshal.load[List[Labelled[SportsSenseInstance]]](FileUtils.readFileToByteArray(trainingDataFile))
    }
    val allDataSize = allData.size
    val testDataSize = math.ceil(allDataSize * .2).toInt
    val trainingData = allData.drop(testDataSize).toIterable
    val testData = allData.take(testDataSize).toIterable
    val naiveBayesModelData = allData.drop(math.ceil(allDataSize * .5).toInt).toIterable
    
    println("Training Data SIze = " + allData.drop(testDataSize).size)
    println("Test Data Size = " + testDataSize)
    println("NB Model Data Size = " + (math.ceil(allDataSize * .5)))
    
    val nbModel = SportsSenseTrainingData.getNBModel(naiveBayesModelData)
    
    for(trainingInstance <- trainingData ){
      trainingInstance.item.naiveBayesScore = Some(nbModel.scores(
          SportsSenseTrainingData.getContextCounter(SolrHelper.getRawDoc(trainingInstance.item.kbpQuery.doc,trainingInstance.item.kbpQuery.year)))
          (true))
    }
    for(testInstance <- testData ){
      testInstance.item.naiveBayesScore = Some(nbModel.scores(
          SportsSenseTrainingData.getContextCounter(SolrHelper.getRawDoc(testInstance.item.kbpQuery.doc,testInstance.item.kbpQuery.year)))
          (true))
    }
    
    val classifier = new SportsClassifier(trainingData)
    
    var classifyOutput = List[(String,Boolean,Double,String,String,String)]()
    for(testInstance <- testData){
      val trueLabel = testInstance.label
      val score = classifier.score(testInstance.item)
      val confidence =  classifier.classifier.getConf(testInstance.item)
      val kbpQuery = testInstance.item.kbpQuery
      classifyOutput = (kbpQuery.name,trueLabel,score,kbpQuery.sourceContext,kbpQuery.stanfordNERType,kbpQuery.id) :: classifyOutput
      //println(testInstance.item.kbpQuery.name +"\t" + trueLabel +"\t" + score + "\t" + confidence)
    }
    for( inst <- classifyOutput.toList.toSeq.sortBy(f => f._3)){
      println(inst._1 +"\t" + inst._2 +"\t" + inst._3 + "\t" + inst._4 +"\t" + inst._5 +"\t" + inst._6)
    }
    
    println(classifier.classifier.intercept)
    
    def precRecall(sorted: Seq[Boolean]): Seq[Double] = {

      var result: List[Double] = Nil

      var total = 0
      var correct = 0

      for (label <- sorted) {
        total += 1
        if (label) {
          correct += 1
        }
        result ::= (correct.toDouble / total.toDouble)
      }
      result.reverse.tails.filter(_.nonEmpty).toSeq.map { tail => tail.max }
    }
    
    val sortedClassifyOutput = classifyOutput.toList.toSeq.sortBy(f => f._3).toList
    val sortedBooleans = sortedClassifyOutput.map(f => f._2)

    val precsItems = precRecall(sortedBooleans).zip(classifyOutput)

    precsItems.zipWithIndex foreach { case ((prec, (name,label,score,sourcecontext,nerType,id)), index) => 
      val recall = index.toDouble / precsItems.size.toDouble
      val recString = "%.02f".format(recall)
      val precString = "%.02f".format(prec)
      println(precString + "\t" + recString + "\t" + score)
    }
  }
  
  
  def writeClassifier(baseDir: String){

    val trainingData2012 =SportsSenseTrainingData.getTrainingData(baseDir, "2012")



    var nbModelOption : Option[NaiveBayes[Boolean,String]] = None
    //val sportsNaiveBayesModelFile = new File("sportsNaiveBayes.model")
    val sportsNaiveBayesModelFile = new File(getClass.getResource("sportsNaiveBayes.model").getPath())
    if(!sportsNaiveBayesModelFile.exists()){
        val naiveBayesModelData = Random.shuffle(trainingData2012).drop(math.ceil(trainingData2012.size * .5).toInt).toIterable
	    nbModelOption = Some(SportsSenseTrainingData.getNBModel(naiveBayesModelData))
	    val modelWriter = new FileOutputStream(sportsNaiveBayesModelFile);
	    modelWriter.write(scala.util.Marshal.dump(nbModelOption.get));
    }
    else{
      println("LOADING NB MODEL")
      nbModelOption = Some(scala.util.Marshal.load[breeze.classify.NaiveBayes[Boolean,String]](FileUtils.readFileToByteArray(sportsNaiveBayesModelFile)))
    }
    
    val nbModel = nbModelOption.get
    

    
    val sportsClassifier2012file = new File("sportsClassifier2012.model")
    if(!sportsClassifier2012file.exists()){
      println("Trying to save model")
      val trainingData2012 =SportsSenseTrainingData.getTrainingData(baseDir, "2012")
        //add naiveBayes scores as a feature to training set
	  for(trainingInstance <- trainingData2012 ){
	    trainingInstance.item.naiveBayesScore = Some(nbModel.scores(
	        SportsSenseTrainingData.getContextCounter(SolrHelper.getRawDoc(trainingInstance.item.kbpQuery.doc,trainingInstance.item.kbpQuery.year)))
	        (true))
	  }
      val classifier2012 = new SportsClassifier(trainingData2012)
      val pw = new PrintWriter(sportsClassifier2012file)
      classifier2012.classifier.save(pw)
      pw.close()
    }
    else{
      
    }    
  }
  
}
