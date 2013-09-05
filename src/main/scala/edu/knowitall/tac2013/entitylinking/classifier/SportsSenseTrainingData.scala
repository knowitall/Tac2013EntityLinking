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
import edu.knowitall.tool.conf.Labelled
import edu.knowitall.tac2013.entitylinking.SolrHelper

object SportsSenseTrainingData {
  
  private val stopWords = {
    val url = getClass.getResource("stopwords.txt")
    require(url != null, "Could not find stopwords.txt")
    io.Source.fromURL(url, "UTF8").getLines.flatMap(_.split(",")).map(_.toLowerCase).toSet
  }
  
  private def getData(baseDir: String, year: String, training: Boolean) : List[SportsSenseInstance] = {
    val queries = KBPQuery.getHelper(baseDir, year).parseKBPQueries(getClass.getResource("/edu/knowitall/tac2013/entitylinking/tac_"+year+"_kbp_english_evaluation_entity_linking_queries.xml").getPath()).toSeq
    var dataList = List[SportsSenseInstance]()
    
    val answerUrl = getClass.getResource("/edu/knowitall/tac2013/entitylinking/benchmark/tac_2012_kbp_english_evaluation_entity_linking_query_types.tab")
    val answers = using(Source.fromURL(answerUrl, "UTF8")) { answerSrc => answerSrc.getLines.map(FormattedOutput.readFormattedOutput).toList }
    val queryAnswerList = queries zip answers
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
           (KBPQuery.getHelper(baseDir, year).kbIdToWikiTypeMap.contains(answer.kbLink))&&
           (query.stanfordNERType != "PERSON"))){
    	  	if(training){
	    	  	if(kbEntryIsTeam(baseDir, year, answer.kbLink)){
	    	  		dataList = new SportsSenseInstance(query,Some(true)) :: dataList
	    	  	}
	    	  	else{
	    	  		dataList = new SportsSenseInstance(query,Some(false)) :: dataList
	    	  	}
    	  	}
    	  	else{
    	  	  dataList = new SportsSenseInstance(query,None) :: dataList
    	  	}
      }
    }
    dataList.toList
  }
  
  def getTrainingData(baseDir : String, year: String) : Iterable[Labelled[SportsSenseInstance]] = {

    val trainingList = getData(baseDir,year,true)
    
    var exList = List[Example[Boolean,Counter[String,Double]]]()
    //go over all instances for true and false label
    for(trainingInstance <- trainingList){
      val label = trainingInstance.hasSportsSense.get
      val id = trainingInstance.kbpQuery.id
      val context = SolrHelper.getRawDoc(trainingInstance.kbpQuery.doc,trainingInstance.kbpQuery.year)
      val counter = getContextCounter(context)
      val ex = Example(label,counter)
      exList = ex :: exList
    }
    for( ssi <- trainingList) yield {
      Labelled(ssi.hasSportsSense.get,ssi)
    }
  }
  
  def getNBModel(trainingList: List[SportsSenseInstance]): breeze.classify.NaiveBayes[Boolean,String] ={
    var exList = List[Example[Boolean,Counter[String,Double]]]()
    //go over all instances for true and false label
    for(trainingInstance <- trainingList){
      val label = trainingInstance.hasSportsSense.get
      val id = trainingInstance.kbpQuery.id
      val context = SolrHelper.getRawDoc(trainingInstance.kbpQuery.doc,trainingInstance.kbpQuery.year)
      val counter = getContextCounter(context)
      val ex = Example(label    ,counter)
      exList = ex :: exList
    }
    val nbClassifier = new NaiveBayes(exList.toIterable,.05,.01)
    nbClassifier
  }
  
  def getNBModel(labelledTrainingList: Iterable[Labelled[SportsSenseInstance]]): breeze.classify.NaiveBayes[Boolean,String] = {
      getNBModel(labelledTrainingList.flatMap(f => List(f.item)).toList)
    }
  
  def getTestData(baseDir: String, year: String): Iterable[SportsSenseInstance] = {

    val testList = getData(baseDir,year,false)
    testList.toIterable
  }
  
  
  def main(args: Array[String]){
    val baseDir = args(0)
    
    val trainingData = getTrainingData(baseDir,"2012")
    val testData =  getData(baseDir,"2012",true)
    
    
    val size = trainingData.size
    println("Size = " + size)
    println("Positive Examples = "+trainingData.count(p => p.item.hasSportsSense.get))
    println("Negative Examples = "+trainingData.count(p => !p.item.hasSportsSense.get))
    
    val nbClassifier = getNBModel(trainingData)
    
    var correctPredictions = 0.0
    var predictions = 0.0
    for(testInstance <- testData){
      val query = testInstance.kbpQuery
      val features = getContextCounter(query.sourceContext)
      val trueLabel = testInstance.hasSportsSense.get
      
      if(trueLabel == nbClassifier.classify(features)){
        correctPredictions +=1
      }
      println(query.name + "\t" + trueLabel +"\t" + nbClassifier.classify(features) + "\t" + nbClassifier.scores(features)(trueLabel))
      predictions += 1
      //ssi.nbProbabilitySportsSense = nbClassifier.scores(features.features)(true)
    }
    
    
    println(correctPredictions/predictions)
  }
  
  class SportsSenseInstance(val kbpQuery : KBPQuery, val hasSportsSense: Option[Boolean]) extends Serializable{
    var naiveBayesScore : Option[Double] = None
  }
  
  def kbEntryIsTeam(baseDir: String, year: String, kbId: String): Boolean = {
    val wikiTypeMap = KBPQuery.getHelper(baseDir, year).kbIdToWikiTypeMap
    if(wikiTypeMap.get(kbId).get.contains("team")||
        wikiTypeMap.get(kbId).get.contains("club")){
      true
    }
    else{
      false
    }
  }
  
  
  def getContextCounter(context: String): Counter[String,Double] = {
    val words = breeze.text.tokenize.PTBTokenizer.apply(context).filter(p => p.forall(p => p.isLetter && p.isLower))
    val noStopWords = words.filter(p => !stopWords.contains(p))
    val p = breeze.text.analyze.PorterStemmer
    val c = Counter[String,Double]()	
    for(w <- noStopWords){
      val stemmed = p.apply(w)
      c += Counter(stemmed -> (1.0/noStopWords.size))
    }
    c
  }
}