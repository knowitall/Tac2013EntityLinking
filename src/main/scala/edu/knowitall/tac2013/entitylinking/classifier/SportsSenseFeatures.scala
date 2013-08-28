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
  val regexScorePattern = """\s[0-9]{1,2}-[0-9]{1,2}""".r
  
 
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
      var minDistance = 30.0
      if(contextIndex != -1){
        val inIndex = context.indexOf(" in ", contextIndex - 30)
        if((inIndex != -1) && (contextIndex > inIndex)){
          minDistance = math.min(30.0,math.abs(contextIndex-inIndex))
        }
      }
      if(minDistance < 30){
        minDistance
      }
      else{
        30.0
      }
    }
  }
  
  object locationVariance extends SportsSenseFeature("location variance"){
    def apply(si :SportsSenseInstance) = {
      val queryId = si.kbpQuery.id
      var namedEntityCollectionMap =
      if(queryId.contains("ENG")){
        CorefHelperMethods.get("2012").queryNamedEntityCollectionMap2012.get
      }
      else{
        CorefHelperMethods.get("2011").queryNamedEntityCollectionMap2011.get
      }
      
      try{
        val locations = namedEntityCollectionMap.get(queryId).get.locations
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
      var namedEntityCollectionMap =
      if(queryId.contains("ENG")){
        CorefHelperMethods.get("2012").queryNamedEntityCollectionMap2012.get
      }
      else{
        CorefHelperMethods.get("2011").queryNamedEntityCollectionMap2011.get
      }
      try{
        val organizations = namedEntityCollectionMap.get(queryId).get.organizations
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
        if(numOrganizationsWithLocations == 0){
          numOrganizations.toDouble
        }
        else{
          (numOrganizations.toDouble/numOrganizationsWithLocations)
        }
      }
      catch{
        case e: Exception => {10.0}
      }
      
    }
    
  }
  object contextLength extends SportsSenseFeature ("Length of source context"){
    def apply(si: SportsSenseInstance) = {
      val kbpQuery = si.kbpQuery
      val context = kbpQuery.sourceContext
      val length = context.split(" ").length
      if( length < 10 ){
        0.0
      }
      else{
        1.0
      }
    }
  }
  object contextIncludesFullySpecifiedLocation extends SportsSenseFeature("If the context includes a fully specified location string"){
    def apply(si: SportsSenseInstance) = {
      val kbpQuery = si.kbpQuery
      val queryId = kbpQuery.id
      var namedEntityCollectionMap =
      if(queryId.contains("ENG")){
        CorefHelperMethods.get("2012").queryNamedEntityCollectionMap2012.get
      }
      else{
        CorefHelperMethods.get("2011").queryNamedEntityCollectionMap2011.get
      }
      val queryName = kbpQuery.name
      val context = kbpQuery.sourceContext
      val fullySpecifiedLocations = namedEntityCollectionMap.get(queryId).get.locations.filter(p => p.contains(",")).
      								filter(p => p.split(",")(0).trim().toLowerCase().contains(queryName.toLowerCase())).
      								filter(p => {context.toLowerCase().contains(p.split(",")(1).trim().toLowerCase())})
//      for(loc <- fullySpecifiedLocations){
//        val part1 = loc.split(",")(0).trim().toLowerCase()
//        val part2 = loc.split(",")(1).trim()
//        
//        var part1Cities = List[String]()
//        val part1Tokens = part1.split(" ")
//        val part1TokensSize = part1Tokens.size
//        var tokensIndex = part1TokensSize-1
//        while(tokensIndex > -1){
//          val cityCandidate = part1Tokens.slice(tokensIndex,part1TokensSize).mkString(" ")
//          if(TipsterData.cities.contains(cityCandidate)){
//            part1Cities = cityCandidate :: part1Cities
//          }
//          tokensIndex -= 1
//        }
//        
//        var country = ""
//        var stateOrProvince = ""
//        if(TipsterData.stateOrProvinces.contains(part2.toLowerCase())){
//          stateOrProvince = part2
//        }
//        if(TipsterData.countries.contains(part2.toLowerCase())){
//          country = part2
//        }
//        
//        for(city <- part1Cities){
//          val cityCased = CorefHelperMethods.locationCasing(city)
//          val expansion = TipsterData.expandStateAbbreviation(part2.toIterator.filter(p => !p.isLetter).mkString(""),cityCased)
//          if(expansion.isDefined && stateOrProvince == ""){
//            stateOrProvince = expansion.get
//          }
//          
//          if(country != ""){
//	          val countryCased = CorefHelperMethods.locationCasing(country)
//	          val countryCitySet = TipsterData.countryCityMap.get(country)
//	          if(countryCitySet.isDefined){
//	            if(countryCitySet.get.contains(cityCased)){
//	              noFullySpecifiedLocation = false
//	            }
//	          }
//          }
//          
//          if(stateOrProvince != ""){
//	          val stateOrProvinceCased = CorefHelperMethods.locationCasing(stateOrProvince)
//	          val stateCitySet = TipsterData.provinceCityMap.get(stateOrProvince)
//	          if(stateCitySet.isDefined){
//	            if(stateCitySet.get.contains(cityCased)){
//	              noFullySpecifiedLocation = false
//	            }
//	          }
//          }
//
//        }
//      }
      println(si.kbpQuery.id + " " + si.kbpQuery.sourceContext)
      if(fullySpecifiedLocations.isEmpty){
        //check heuristic for possible location
        val words = context.split(" ")
        var wordIndex = 0
        while(wordIndex < (words.length -1)){
          val w = words(wordIndex)
          if(w.contains(queryName) && w.endsWith(",") && words(wordIndex+1)(0).isUpper){
            1.0
          }
          wordIndex +=1
        }
        0.0
      }
      else{
        1.0
      }
    }
  }
  
  
  //private val features = Seq(contextIncludesFullySpecifiedLocation,organizationsWithLocationStrings,locationVariance,inProximity,scorePattern,nbScore,isStanfordNone,isStanfordLocation,isStanfordOrganization,isStanfordPerson)
  //  private val features = Seq(organizationsWithLocationStrings,locationVariance,inProximity,scorePattern,nbScore,isStanfordNone,isStanfordLocation,isStanfordOrganization,isStanfordPerson)
  //private val features = Seq(locationVariance,inProximity,scorePattern,nbScore,isStanfordNone,isStanfordLocation,isStanfordOrganization,isStanfordPerson) //** best so far
  //private val features = Seq(inProximity,scorePattern,nbScore,isStanfordNone,isStanfordLocation,isStanfordOrganization,isStanfordPerson)
  //private val features = Seq(nbScore,isStanfordNone,isStanfordLocation,isStanfordOrganization,isStanfordPerson) ** STRONG SCORE
  private val features = Seq(scorePattern,organizationsWithLocationStrings,contextLength,contextIncludesFullySpecifiedLocation,inProximity,nbScore,isStanfordNone,isStanfordLocation,isStanfordOrganization,isStanfordPerson)

  
  def featureSet = new FeatureSet(SortedMap.empty[String, Feature[SportsSenseInstance, Double]] ++ features.map(f => (f.name, f)).toMap) 
}