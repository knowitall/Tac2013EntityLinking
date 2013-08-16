package edu.knowitall.tac2013.entitylinking.classifier

import edu.knowitall.tool.conf.Feature
import edu.knowitall.tool.conf.FeatureSet
import scala.collection.immutable.SortedMap
import edu.knowitall.tac2013.entitylinking.classifier.SportsSenseTrainingData.SportsSenseInstance
import edu.knowitall.taggers.tag.TaggerCollection
import edu.knowitall.tool.chunk.OpenNlpChunker
import edu.knowitall.tool.stem.Lemmatized

object SportsSenseFeatures {

  val tc = TaggerCollection.fromPath(getClass.getResource("dateTaggers").getPath())
  val chunker = new OpenNlpChunker()
  val numPattern = """[0-9]+""".r
  
 
  type SportsSenseFeature = Feature[SportsSenseInstance, Double]
  
  object isStanfordOrganization extends SportsSenseFeature("NER organization"){
    def apply(si: SportsSenseInstance) = {
      if(si.kbpQuery.stanfordNERType == "ORGANIZATION"){
        1.0
      }
      else{
        0.0
      }
    }
  }
  
  object isStanfordPerson extends SportsSenseFeature("NER organization"){
    def apply(si: SportsSenseInstance) = {
      if(si.kbpQuery.stanfordNERType == "PERSON"){
        1.0
      }
      else{
        0.0
      }
    }
  }
  
  object isStanfordLocation extends SportsSenseFeature("NER organization"){
    def apply(si: SportsSenseInstance) = {
      if(si.kbpQuery.stanfordNERType == "LOCATION"){
        1.0
      }
      else{
        0.0
      }
    }
  }
  
  object isStanfordNone extends SportsSenseFeature("NER organization"){
    def apply(si: SportsSenseInstance) = {
      if(si.kbpQuery.stanfordNERType == "None"){
        1.0
      }
      else{
        0.0
      }
    }
  }

  object nbScore extends SportsSenseFeature("docsim score") {
    def apply(si: SportsSenseInstance) = {
      si.nbProbabilitySportsSense
    }
  }
  
  object numNumbers extends SportsSenseFeature("number of Numbers"){
    def apply(si :SportsSenseInstance) = {
      val context = si.kbpQuery.sourceWideContext
      //context.size
      //chunker.chunk(context)
      val numberOfDigits = numPattern.findAllMatchIn(context).size
      (numberOfDigits/context.size)
    }
  }

  private val features = Seq(nbScore,numNumbers,isStanfordNone,isStanfordLocation,isStanfordOrganization,isStanfordPerson)
  
  def featureSet = new FeatureSet(SortedMap.empty[String, Feature[SportsSenseInstance, Double]] ++ features.map(f => (f.name, f)).toMap) 
}