package edu.knowitall.tac2013.entitylinking.utils

import edu.knowitall.tac2013.entitylinking.KBPQuery

object SportsHelperMethods {
  
  def findTeamEntity(queryName: String):Option[String]= {
     if(queryName == "Copenhagen"){
              println("Searching for Copenhagen sports team")
            }
    val wikiMap = KBPQuery.wikiMap.get
    val wikiTypeMap = KBPQuery.kbIdToWikiTypeMap.get
    
    var candidateLinks = List[String]()
    for(entry <- wikiMap){
      val name = entry._1
      if(name.contains(queryName)){
        candidateLinks = entry._2 :: candidateLinks
      }
    }
    
    for(candidateLink <- candidateLinks){
      val typeInformation = wikiTypeMap.get(candidateLink)
      if(typeInformation.isDefined){
        if(typeInformation.get.contains("team") ||
            typeInformation.get.contains("club") &&
            !typeInformation.get.contains("season")){
          return Some(candidateLink)
        }
      }
    }
    None
  }
  
  def isLocation(kbLink: String ):Boolean = {
     val wikiTypeMap = KBPQuery.kbIdToWikiTypeMap.get
     val wikiStructuredTypeMap = KBPQuery.kbIdToWikiStructuredTypeMap.get
     val wikiStructuredType = wikiStructuredTypeMap.get(kbLink).getOrElse("")
     val wikiType = wikiTypeMap.get(kbLink).getOrElse("").toLowerCase()
     if(wikiType.contains("settlement") || wikiType.contains("city") || wikiType.contains("place") || wikiType.contains("province")
         || wikiType.contains("country") || wikiType.contains("county") || 
         wikiStructuredType == "GPE"){
       return true
     }
     false
  }

}