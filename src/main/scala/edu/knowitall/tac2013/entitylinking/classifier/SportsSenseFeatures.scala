package edu.knowitall.tac2013.entitylinking.classifier

import edu.knowitall.tool.conf.Feature
import edu.knowitall.tool.conf.FeatureSet
import scala.collection.immutable.SortedMap
import edu.knowitall.taggers.tag.TaggerCollection
import edu.knowitall.tool.chunk.OpenNlpChunker
import edu.knowitall.tool.stem.Lemmatized
import edu.knowitall.tac2013.entitylinking.classifier.SportsSenseTrainingData.SportsSenseInstance
import edu.knowitall.tac2013.entitylinking.coref.CorefHelperMethods
import edu.knowitall.tac2013.entitylinking.utils.TipsterData

object SportsSenseFeatures {

  //val tc = TaggerCollection.fromPath(getClass.getResource("dateTaggers").getPath())
  val chunker = new OpenNlpChunker()
  val numPattern = """[0-9]+""".r
  val regexScorePattern = """[0-9]{1,2}-[0-9]{1,2}""".r
  
 
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
  
  object isStanfordPerson extends SportsSenseFeature("NER person"){
    def apply(si: SportsSenseInstance) = {
      if(si.kbpQuery.stanfordNERType == "PERSON"){
        1.0
      }
      else{
        0.0
      }
    }
  }
  
  object isStanfordLocation extends SportsSenseFeature("NER location"){
    def apply(si: SportsSenseInstance) = {
      if(si.kbpQuery.stanfordNERType == "LOCATION"){
        1.0
      }
      else{
        0.0
      }
    }
  }
  
  object isStanfordNone extends SportsSenseFeature("NER none"){
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
      if(si.naiveBayesScore.isDefined){
        val logProb = si.naiveBayesScore.get
        val normProb = math.pow(math.E, logProb)
        logProb
      }
      else{
        0.0
      }
    }
  }
  
  object numNumbers extends SportsSenseFeature("number of Numbers"){
    def apply(si :SportsSenseInstance) = {
      val context = si.kbpQuery.sourceContext
      var contextSize = context.size
      if(contextSize == 0){
        contextSize = 1
      }
      //context.size
      //chunker.chunk(context)
      val numberOfDigits = numPattern.findAllMatchIn(context).size
      (numberOfDigits/contextSize)
    }
  }
  
  object scorePattern extends SportsSenseFeature("score pattern matches"){
    def apply(si: SportsSenseInstance) = {
      val context = si.kbpQuery.sourceContext
      var contextSize = context.size
      if(contextSize == 0){
        contextSize = 1
      }
      //context.size
      //chunker.chunk(context)
      val numberOfScores= regexScorePattern.findAllMatchIn(context).size
      (numberOfScores/contextSize)
    }
  }
  
  object inProximity extends SportsSenseFeature("proximity of preposition in"){
    def apply(si: SportsSenseInstance)= {
      val context = si.kbpQuery.sourceContext
      val contextIndex = context.indexOf(si.kbpQuery.name)
      var minDistance = 20.0
      if(contextIndex != -1){
        val inIndex = context.indexOf(" in ", contextIndex - 20)
        if((inIndex != -1) && (contextIndex > inIndex)){
          minDistance = math.min(20.0,math.abs(contextIndex-inIndex))
        }
      }
      if(minDistance < 20){
        1.0
      }
      else{
        0.0
      }
    }
  }
  
  object locationVariance extends SportsSenseFeature("location variance"){
    def apply(si :SportsSenseInstance) = {
      val queryId = si.kbpQuery.id
      var namedEntityCollectionMap :Option[Map[String,CorefHelperMethods.NamedEntityCollection]] = None
      if(queryId.contains("ENG")){
        namedEntityCollectionMap = CorefHelperMethods.queryNamedEntityCollectionMap2012
      }
      else{
        namedEntityCollectionMap = CorefHelperMethods.queryNamedEntityCollectionMap2011
      }
      
      try{
        val locations = namedEntityCollectionMap.get.get(queryId).get.locations
        val numLocations = locations.size
        val numUnique = locations.groupBy(f => {f}).size
        if(numLocations == 0){
          1.0
        }
        else{

          (numUnique/numLocations.toDouble)
        }
        
      }
      catch{
        case e: Exception => {1.0}
      }
      
    }
  }
  
  object organizationsWithLocationStrings extends SportsSenseFeature("Ratio of Organizations With Locations to Other Organizations"){
    def apply(si: SportsSenseInstance) = {
      val queryId = si.kbpQuery.id
      var namedEntityCollectionMap :Option[Map[String,CorefHelperMethods.NamedEntityCollection]] = None
      if(queryId.contains("ENG")){
        namedEntityCollectionMap = CorefHelperMethods.queryNamedEntityCollectionMap2012
      }
      else{
        namedEntityCollectionMap = CorefHelperMethods.queryNamedEntityCollectionMap2011
      }
      try{
        val organizations = namedEntityCollectionMap.get.get(queryId).get.organizations
        val numOrganizations= organizations.size
        val organizationCopy= organizations
        var numOrganizationsWithLocations = 0.0
        for(ol <- organizationCopy){
          var includesLocation = false
          val olWords = ol.split(" ")
          for(olWord <- olWords){
	          if( (TipsterData.cities.contains(olWord.toLowerCase()))||
	              (TipsterData.stateOrProvinces.contains(olWord.toLowerCase())) ||
	              (TipsterData.countries.contains(olWord.toLowerCase()))){
	            includesLocation = true
	          }
          }
          if(includesLocation){
            numOrganizationsWithLocations +=1 
          }
        }
        if(numOrganizations == 0){
          1.0
        }
        else{
          (numOrganizationsWithLocations/numOrganizations.toDouble)
        }
      }
      catch{
        case e: Exception => {1.0}
      }
      
    }
    
  }
  
  object contextIncludesFullySpecifiedLocation extends SportsSenseFeature("If the context includes a fully specified location string"){
    def apply(si: SportsSenseInstance) = {
      val kbpQuery = si.kbpQuery
      val queryId = kbpQuery.id
      var namedEntityCollectionMap :Option[Map[String,CorefHelperMethods.NamedEntityCollection]] = None
      if(queryId.contains("ENG")){
        namedEntityCollectionMap = CorefHelperMethods.queryNamedEntityCollectionMap2012
      }
      else{
        namedEntityCollectionMap = CorefHelperMethods.queryNamedEntityCollectionMap2011
      }
      val queryName = kbpQuery.name
      val context = kbpQuery.sourceContext
      val fullySpecifiedLocations = namedEntityCollectionMap.get.get(queryId).get.locations.filter(p => p.contains(",")).
      								filter(p => p.split(",")(0).trim().toLowerCase().contains(queryName.toLowerCase())).
      								filter(p => {context.contains(p.split(",")(1).trim().toLowerCase())})
      var noFullySpecifiedLocation = true
      for(loc <- fullySpecifiedLocations){
        val part1 = loc.split(",")(0).trim().toLowerCase()
        val part2 = loc.split(",")(1).trim()
        
        var part1Cities = List[String]()
        val part1Tokens = part1.split(" ")
        val part1TokensSize = part1Tokens.size
        var tokensIndex = part1TokensSize-1
        while(tokensIndex > -1){
          val cityCandidate = part1Tokens.slice(tokensIndex,part1TokensSize).mkString(" ")
          if(TipsterData.cities.contains(cityCandidate)){
            part1Cities = cityCandidate :: part1Cities
          }
          tokensIndex -= 1
        }
        
        var country = ""
        var stateOrProvince = ""
        if(TipsterData.stateOrProvinces.contains(part2.toLowerCase())){
          stateOrProvince = part2
        }
        if(TipsterData.countries.contains(part2.toLowerCase())){
          country = part2
        }
        
        for(city <- part1Cities){
          val cityCased = CorefHelperMethods.locationCasing(city)
          val expansion = TipsterData.expandStateAbbreviation(part2.toIterator.filter(p => !p.isLetter).mkString(""),cityCased)
          if(expansion.isDefined && stateOrProvince == ""){
            stateOrProvince = expansion.get
          }
          
          if(country != ""){
	          val countryCased = CorefHelperMethods.locationCasing(country)
	          val countryCitySet = TipsterData.countryCityMap.get(country)
	          if(countryCitySet.isDefined){
	            if(countryCitySet.get.contains(cityCased)){
	              noFullySpecifiedLocation = false
	            }
	          }
          }
          
          if(stateOrProvince != ""){
	          val stateOrProvinceCased = CorefHelperMethods.locationCasing(stateOrProvince)
	          val stateCitySet = TipsterData.provinceCityMap.get(stateOrProvince)
	          if(stateCitySet.isDefined){
	            if(stateCitySet.get.contains(cityCased)){
	              noFullySpecifiedLocation = false
	            }
	          }
          }

        }
      }
      if(noFullySpecifiedLocation){
        1.0
      }
      else{
        0.0
      }
    }
  }
  
  
  private val features = Seq(contextIncludesFullySpecifiedLocation,organizationsWithLocationStrings,locationVariance,inProximity,scorePattern,nbScore,isStanfordNone,isStanfordLocation,isStanfordOrganization,isStanfordPerson)

  
  def featureSet = new FeatureSet(SortedMap.empty[String, Feature[SportsSenseInstance, Double]] ++ features.map(f => (f.name, f)).toMap) 
}