package edu.knowitall.tac2013.entitylinking.classifier

import edu.knowitall.tac2013.entitylinking.KBPQuery
import edu.knowitall.tac2013.entitylinking.KBPQueryHelper
import edu.knowitall.common.Resource.using
import scala.io.Source
import edu.knowitall.tac2013.entitylinking.FormattedOutput
import edu.knowitall.tac2013.entitylinking.utils.TipsterData
import edu.knowitall.tac2013.entitylinking.utils.FormattedOutputToHumanReadableOutputConverter
import breeze.classify.NaiveBayes
import breeze.data.Example
import breeze.linalg.Counter

object SportsSenseTrainingData {
  
  
  def main(args: Array[String]){
    val baseDir = args(0)
    
    val helper2012 = KBPQuery.getHelper(baseDir, "2012")
    val queries = helper2012.parseKBPQueries(getClass.getResource("/edu/knowitall/tac2013/entitylinking/tac_2012_kbp_english_evaluation_entity_linking_queries.xml").getPath()).toSeq
    
    val answerUrl = getClass.getResource("/edu/knowitall/tac2013/entitylinking/benchmark/tac_2012_kbp_english_evaluation_entity_linking_query_types.tab")
    val answers = using(Source.fromURL(answerUrl, "UTF8")) { answerSrc => answerSrc.getLines.map(FormattedOutput.readFormattedOutput).toList }
    val queryAnswerList = queries zip answers
    var trainingList = List[SportsSenseInstance]()
    for(queryAnswer <- queryAnswerList){
      val query = queryAnswer._1
      val answer = queryAnswer._2
      val queryName = query.name
      //if it's a location name and it links to the KB
      //then it should be part of training data
      if( (TipsterData.cities.contains(queryName.toLowerCase()) ||
          TipsterData.stateOrProvinces.contains(queryName.toLowerCase()) ||
          TipsterData.countries.contains(queryName.toLowerCase())) &&
          (!answer.kbLink.startsWith("NIL") &&
           (helper2012.kbIdToWikiTypeMap.contains(answer.kbLink)) &&
           (query.stanfordNERType != "PERSON"))){
        if(kbEntryIsTeam(helper2012, answer.kbLink)){
          trainingList = new SportsSenseInstance(query,true) :: trainingList
        }
        else{
          trainingList = new SportsSenseInstance(query,false) :: trainingList
        }
      }
    }
    
    val helper2011 = KBPQuery.getHelper(baseDir, "2011")
    val queries2011 = helper2011.parseKBPQueries(getClass.getResource("/edu/knowitall/tac2013/entitylinking/tac_2011_kbp_english_evaluation_entity_linking_queries.xml").getPath()).toSeq
    val answerUrl2011 = getClass.getResource("/edu/knowitall/tac2013/entitylinking/benchmark/tac_2011_kbp_english_evaluation_entity_linking_query_types.tab")
    val answers2011 = using(Source.fromURL(answerUrl, "UTF8")) { answerSrc => answerSrc.getLines.map(FormattedOutput.readFormattedOutput).toList }
    val queryAnswerList2011 = queries2011 zip answers2011
    for(queryAnswer <- queryAnswerList2011){
      val query = queryAnswer._1
      val answer = queryAnswer._2
      val queryName = query.name
      //if it's a location name and it links to the KB
      //then it should be part of training data
      if( (TipsterData.cities.contains(queryName.toLowerCase()) ||
          TipsterData.stateOrProvinces.contains(queryName.toLowerCase()) ||
          TipsterData.countries.contains(queryName.toLowerCase())) &&
          (!answer.kbLink.startsWith("NIL") &&
           (helper2011.kbIdToWikiTypeMap.contains(answer.kbLink))&&
           (query.stanfordNERType != "PERSON"))){
        if(kbEntryIsTeam(helper2011, answer.kbLink)){
          trainingList = new SportsSenseInstance(query,true) :: trainingList
        }
        else{
          trainingList = new SportsSenseInstance(query,false) :: trainingList
        }
      }
    }
    
    
    val size = trainingList.size
    println("Size = " + size)
    println("Positive Examples = "+trainingList.count(p => p.hasSportsSense))
    println("Negative Examples = "+trainingList.count(p => !p.hasSportsSense))
    
    
    //use naive bayes bag of words to get probabilities
    
    var exList = List[Example[Boolean,Counter[String,Double]]]()
    //go over all instances for true and false label
    for(trainingInstance <- trainingList){
      val label = trainingInstance.hasSportsSense
      val id = trainingInstance.kbpQuery.id
      val context = trainingInstance.kbpQuery.sourceWideContext
      val counter = getContextCounter(context)
      val ex = Example(label    ,counter)
      exList = ex :: exList
    }
    val instanceAndNBData = exList zip trainingList
    
    val nbClassifier = new NaiveBayes(exList.toIterable,.05,.01)
    
    var correctPredictions = 0.0
    var predictions = 0.0
    for(testInstance <- instanceAndNBData){
      val features = testInstance._1
      val ssi = testInstance._2
      if(ssi.hasSportsSense == nbClassifier.classify(features.features)){
        correctPredictions +=1
      }
      println(ssi.kbpQuery.name + "\t" + ssi.hasSportsSense +"\t" + nbClassifier.classify(features.features) + "\t" + nbClassifier.scores(features.features)(ssi.hasSportsSense))
      predictions += 1
      ssi.nbProbabilitySportsSense = nbClassifier.scores(features.features)(true)
    }
    
    println(correctPredictions/predictions)
  }
  
  class SportsSenseInstance(val kbpQuery : KBPQuery, val hasSportsSense: Boolean){
    
    var nbProbabilitySportsSense = 0.0
  }
  
  
  def kbEntryIsTeam(helper: KBPQueryHelper, kbId: String): Boolean = {
    
    val wikiTypeMap = helper.kbIdToWikiTypeMap
    if(wikiTypeMap.get(kbId).get.contains("team")||
        wikiTypeMap.get(kbId).get.contains("club")){
      true
    }
    else{
      false
    }
  }
  
  def getContextCounter(context: String): Counter[String,Double] = {
    val words = breeze.text.tokenize.PTBTokenizer.apply(context)
    val p = breeze.text.analyze.PorterStemmer
    val c = Counter[String,Double]()
    for(w <- words){
      val stemmed = p.apply(w)
      c += Counter(stemmed -> 1.0)
    }
    c
  }
}