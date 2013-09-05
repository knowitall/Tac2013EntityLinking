package edu.knowitall.tac2013.entitylinking.classifier

import edu.knowitall.tac2013.entitylinking.KBPQuery
import breeze.classify.NaiveBayes
import org.apache.commons.io.FileUtils
import java.io.File
import edu.knowitall.tool.conf.impl.LogisticRegression
import edu.knowitall.tac2013.entitylinking.classifier.SportsSenseTrainingData.SportsSenseInstance
import java.net.URL
import edu.knowitall.tac2013.entitylinking.utils.TipsterData
import edu.knowitall.tac2013.entitylinking.SolrHelper

object SportsSenseLabeller {
  
  val sportsNaiveBayesModel2012 = scala.util.Marshal.load[NaiveBayes[Boolean,String]](FileUtils.readFileToByteArray(new File(getClass.getResource("sportsNaiveBayes.model").getPath())))
  val sportsLogisticRegressionClassifier2012 = LogisticRegression.fromUrl[SportsSenseInstance](SportsSenseFeatures.featureSet,new File(getClass.getResource("sportsClassifier2012.model").getPath()).toURI().toURL())
  val threshold = .15
  
  def labelSportsSense(kbpQuery: KBPQuery): Option[Boolean] = {
    if(isCandidateForSportsClassifier(kbpQuery)){
      val sportsSenseInstance = new SportsSenseTrainingData.SportsSenseInstance(kbpQuery,None)
      val nbScore = Some(sportsNaiveBayesModel2012.scores(SportsSenseTrainingData.getContextCounter(SolrHelper.getRawDoc(kbpQuery.doc,kbpQuery.year)))(true))
      sportsSenseInstance.naiveBayesScore = nbScore
      val sportsScore = sportsLogisticRegressionClassifier2012.apply(sportsSenseInstance)
      println(kbpQuery.name + "score: " + sportsScore)
      if(sportsScore > threshold){
        Some(true)
      }
      else{
        Some(false)
      }
    }
    else{
      None
    }
  }
  
  def isCandidateForSportsClassifier(kbpQuery: KBPQuery): Boolean = {
      val queryName = kbpQuery.name
      if( (TipsterData.cities.contains(queryName.toLowerCase()) ||
    		  TipsterData.stateOrProvinces.contains(queryName.toLowerCase()) ||
    		  TipsterData.countries.contains(queryName.toLowerCase())) &&
    		  (kbpQuery.stanfordNERType != "PERSON")){
    	  					true
      		  }
      else{
        false
      }
  }

}