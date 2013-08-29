package edu.knowitall.tac2013.entitylinking.classifier

import edu.knowitall.tool.conf.BreezeLogisticRegressionTrainer
import edu.knowitall.tool.conf.ConfidenceTrainer
import edu.knowitall.tool.conf.ConfidenceFunction
import edu.knowitall.tool.conf.Labelled
import edu.knowitall.tool.conf.FeatureSet
import edu.knowitall.browser.entity.EntityLink
import edu.knowitall.tac2013.entitylinking.KBPQuery

class LinkClassifier(val trainingData: Iterable[Labelled[KBPQueryLink]]) {
  
  def this(baseDir: String){
    this(new LinkTrainingData(baseDir))
  }
   
  val trainer = new BreezeLogisticRegressionTrainer(LinkFeatures.featureSet)
  
  val classifier = trainer.train(trainingData)
  
  def score(link: KBPQueryLink): Double = classifier(link) 
  
  def score(query: KBPQuery, link: EntityLink) = classifier(new KBPQueryLink(query, link))
}

object LinkClassifierTest {

  def main(args: Array[String]): Unit = {
    
    val allTrainingDataSet = new LinkTrainingData("/scratch/resources/entitylinkingResources", "2012").toSet
    
    require(allTrainingDataSet.exists(_.label == true))
    require(allTrainingDataSet.exists(_.label == false))

    val allTestDataSet = new LinkTrainingData("/scratch/resources/entitylinkingResources", "2011").toSet

    val splits = 10

    val testSize = math.ceil(allTrainingDataSet.size.toDouble / splits.toDouble).toInt

    val testSets = Seq(allTestDataSet) // allTrainingDataSet.toSeq.grouped(testSize).map(_.toSet)

    val trainTestSets = Seq((allTrainingDataSet, allTestDataSet)) // testSets.map(tset => (allTrainingDataSet &~ tset, tset))

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

    println(allTrainingDataSet.size)

    val scoredTestUnflattened = for ((train, test) <- trainTestSets) yield {
      val classifier = new LinkClassifier(train)
      val scores = test.map(l => (l, classifier.score(l.item)))
      scores
    }

    val scoredTest = scoredTestUnflattened.flatten

    val sortedTest = scoredTest.toSeq.sortBy(-_._2)

    val sortedBooleans = sortedTest.map(_._1.label)

    val precsItems = precRecall(sortedBooleans).zip(sortedTest)

    val output = new java.io.PrintStream("classifier-out-2012vs20115.txt")

    precsItems.zipWithIndex foreach { case ((prec, (litem, conf)), index) => 
      val recall = index.toDouble / precsItems.size.toDouble
      val recString = "%.02f".format(recall)
      val precString = "%.02f".format(prec)
      output.println(precString + "\t" + recString + "\t" + conf)
    }
  }
}