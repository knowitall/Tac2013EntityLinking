package edu.knowitall.tac2013.entitylinking.classifier

import edu.knowitall.tool.conf.BreezeLogisticRegressionTrainer
import edu.knowitall.tool.conf.ConfidenceTrainer
import edu.knowitall.tool.conf.ConfidenceFunction
import edu.knowitall.tool.conf.Labelled
import edu.knowitall.tool.conf.FeatureSet
import edu.knowitall.browser.entity.EntityLink

class LinkClassifier(val trainingData: Iterable[Labelled[EntityLink]] = new LinkTrainingData()) {
 
  val trainer = new BreezeLogisticRegressionTrainer(LinkFeatures.featureSet)
  
  val classifier = trainer.train(trainingData)
  
  def score(link: EntityLink): Double = classifier(link) 
}

object LinkClassifierTest {

  def main(args: Array[String]): Unit = {

    val allTrainingDataSet = new LinkTrainingData().toSet

    val splits = 10

    val testSize = math.ceil(allTrainingDataSet.size.toDouble / splits.toDouble).toInt

    val testSets = allTrainingDataSet.toSeq.grouped(testSize).map(_.toSet)

    val trainTestSets = testSets.map(tset => (allTrainingDataSet &~ tset, tset))

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

    precsItems.zipWithIndex foreach { case ((prec, (litem, conf)), index) => 
      val recall = index.toDouble / precsItems.size.toDouble
      val recString = "%.02f".format(recall)
      val precString = "%.02f".format(prec)
      println(precString + "\t" + recString + "\t" + conf)
    }
  }
}