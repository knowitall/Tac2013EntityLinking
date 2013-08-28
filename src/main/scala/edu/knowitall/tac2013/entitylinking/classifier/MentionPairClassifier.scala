package edu.knowitall.tac2013.entitylinking.classifier

import edu.knowitall.tool.conf.BreezeLogisticRegressionTrainer
import edu.knowitall.tool.conf.ConfidenceTrainer
import edu.knowitall.tool.conf.ConfidenceFunction
import edu.knowitall.tool.conf.Labelled
import edu.knowitall.tool.conf.FeatureSet
import edu.knowitall.common.Timing
import edu.knowitall.browser.entity.EntityLink

class MentionPairClassifier(val trainingData: Iterable[Labelled[MentionPair]]) {
 
  val trainer = new BreezeLogisticRegressionTrainer(MentionPairFeatures.featureSet)

  val classifier = {
    val (nsLoad, c) = Timing.time {
      System.err.println("Loading mention pair classifier...")
      trainer.train(trainingData)
    }
    System.err.println("Loaded classifier in: " + Timing.Seconds.format(nsLoad))
    c
  }
  
  def score(pair: MentionPair): Double = classifier(pair) 
}

object MentionPairClassifier {
  lazy val default = new MentionPairClassifier(new MentionPairTrainingData("/scratch/", "2012"))
}

object MentionPairClassifierTest {

  def main(args: Array[String]): Unit = {

    val allTrainingDataSet = new MentionPairTrainingData("/scratch/", "2012").toSet

    require(allTrainingDataSet.exists(_.label == true))
    require(allTrainingDataSet.exists(_.label == false))
    
    val allTestDataSet = allTrainingDataSet//new MentionPairTrainingData("/scratch/", "2011").toSet

    val xValSplits = 8
    val xValSplitSize = math.ceil(allTrainingDataSet.size.toDouble / xValSplits.toDouble).toInt
    lazy val testSets = allTrainingDataSet.iterator.grouped(xValSplitSize).map(_.toSet).toSeq
    lazy val trainSets = testSets.map(tset => allTrainingDataSet &~ tset)
    
    require(allTestDataSet.exists(_.label == true))
    require(allTestDataSet.exists(_.label == false))
    
    val trainTestSets = trainSets.zip(testSets) // Seq((allTrainingDataSet, allTestDataSet))

    def precRecall(sorted: Seq[Boolean]): Seq[Double] = {

      var result: List[Double] = Nil

      var total = 0
      var correct = 0

      for (label <- sorted) {
        total += 1
        if (label) {
          correct += 1
          result ::= (correct.toDouble / total.toDouble)
        }
      }
      result.reverse.tails.filter(_.nonEmpty).toSeq.map { tail => tail.max }
    }

    println(allTrainingDataSet.size)
    println(allTrainingDataSet.filter(_.label == true).size)
    println(allTrainingDataSet.filter(_.label == false).size)
    println(allTestDataSet.size)
    println(allTestDataSet.filter(_.label == true).size)
    println(allTestDataSet.filter(_.label == false).size)
    
    val output = new java.io.PrintStream("classifiertest2012_self.csv")
    
    val scoredTestUnflattened = for (((train, test), index) <- trainTestSets.zipWithIndex) yield {
      System.err.println("Doing X-val run #"+index)
      val classifier = new MentionPairClassifier(train)
      classifier.classifier.featureWeights.iterator.foreach(output.println)
      
      val scores = test.map(l => (l, classifier.score(l.item)))
      scores
    }

    val scoredTest = scoredTestUnflattened.flatten

    val sortedTest = scoredTest.toSeq.sortBy(-_._2)

    val sortedBooleans = sortedTest.map(_._1.label)

    val precsItems = precRecall(sortedBooleans).zip(sortedTest.filter(_._1.label == true))

    
    
    output.println(MentionPair.header)
    
    sortedTest.foreach { case (Labelled(label, item), score) =>
      output.println(label + "\t" + score + "\t" + item) 
    }
    
    output.println
    
    precsItems.zipWithIndex foreach { case ((prec, (litem, conf)), index) => 
      val recall = index.toDouble / precsItems.size.toDouble
      val recString = "%.02f".format(recall)
      val precString = "%.02f".format(prec)
      output.println(precString + "\t" + recString + "\t" + conf)
    }
    
    output.println()
  }
}