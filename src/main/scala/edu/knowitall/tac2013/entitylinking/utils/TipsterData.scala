package edu.knowitall.tac2013.entitylinking.utils


object TipsterData {
  
  private val tipsterResourcePath = "/edu/knowitall/tac2013/entitylinking/TipsterGazetteer.txt"
  private val cityProvincePattern = """([^\(]+)\(CITY.+?([^\(\)]+)\(PROVINCE.*""".r
  private val cityCountryPattern = """([^\(]+)\(CITY.+?([^\(\)]+)\(COUNTRY.*""".r
  
  private val tipsterURL = getClass().getResource(tipsterResourcePath)
  require(tipsterURL != null, "Could not find resource: " + tipsterResourcePath)

    
  val citySet =  scala.collection.mutable.Set[String]()
  val stateOrProvinceSet = scala.collection.mutable.Set[String]()
  val countrySet = scala.collection.mutable.Set[String]()
  
  val provinceCityMap = scala.collection.mutable.Map[String,Set[String]]()
  val countryCityMap = scala.collection.mutable.Map[String,Set[String]]()


  // read in tipster lines with latin encoding so as not to get errors.
  scala.io.Source.fromFile(tipsterURL.getPath())(scala.io.Codec.ISO8859).getLines.foreach(line => {
      val cityProvinceMatch = cityProvincePattern.findFirstMatchIn(line)
      if(cityProvinceMatch.isDefined){
        val city = cityProvinceMatch.get.group(1).trim()
        val province = cityProvinceMatch.get.group(2).trim()
        
        if(provinceCityMap.contains(province)){
          val oldSet = provinceCityMap.get(province)
          provinceCityMap += ((province, oldSet.get + city))
        }
        else{
          provinceCityMap += ((province, Set(city)))
        }
      }
      
      val cityCountryMatch = cityCountryPattern.findFirstMatchIn(line)
      if(cityCountryMatch.isDefined){
        val city = cityCountryMatch.get.group(1).trim()
        val country = cityCountryMatch.get.group(2).trim()
        
        if(countryCityMap.contains(country)){
          val oldSet = countryCityMap.get(country)
          countryCityMap += ((country, oldSet.get + city))
        }
        else{
          countryCityMap += ((country, Set(city)))
        }
      }
      
      val pairs = line.split("\\)")
      val pairSplits = { for(p <- pairs) yield p.split("\\(")}
      for(nameAndLocationType <- pairSplits){
        if(nameAndLocationType.size ==2){
	        val name = nameAndLocationType(0).trim().toLowerCase()
	        val locationType = nameAndLocationType(1).split(" ")(0).trim()	        
	        locationType match {
	          case "CITY" => { if(!citySet.contains(name)) {
	        	  					citySet.add(name)
	          				}
	          }
	          case "COUNTRY" => { if(!countrySet.contains(name)) {
	        	  					countrySet.add(name)
	        	  				}
	          }
	          case "PROVINCE" => { if(!stateOrProvinceSet.contains(name)){
	        	  				stateOrProvinceSet.add(name)
	          					}
	          }
	          case _ => {}
	        }
        }
      }
      
      
    })
    
   lazy val provinceToCityMap = provinceCityMap.toMap
   lazy val cities = citySet.toSet
   lazy val countries = countrySet.toSet
   lazy val stateOrProvinces = stateOrProvinceSet.toSet
   
   
   def main(args: Array[String]) = {
    val lcities = provinceToCityMap.get("Louisiana").get
    for(c <- lcities){
      println(c)
    }
  }
  
  
  def expandStateAbbreviation(abr: String, city: String) :Option[String] = {
    if(AbbreviationData.abbreviationMap.contains(abr)){
      val stateName = AbbreviationData.abbreviationMap.get(abr)
      if(stateName.isEmpty) return None
      val citiesInState = provinceToCityMap.get(stateName.get)
      if(citiesInState.isEmpty) return None
      if(citiesInState.get.contains(city)){
        println("Transforming " + abr + " to " + stateName.get)
        return Some((city +", " + stateName.get))
      }
      else return None 
    }
    else{
      None
    }
  }
}