package edu.knowitall.tac2013.entitylinking.utils

import edu.knowitall.tac2013.entitylinking.KBPQuery
import scala.collection.mutable

case class SportsHelperMethods(val baseDir: String, val year: String) {
  
  def findTeamEntity(queryName: String):Option[String]= {
     if(queryName == "Copenhagen"){
              println("Searching for Copenhagen sports team")
            }
    val wikiMap = KBPQuery.getHelper(baseDir, year).wikiMap
    val wikiTypeMap = KBPQuery.getHelper(baseDir, year).kbIdToWikiTypeMap
    
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
     val wikiTypeMap = KBPQuery.getHelper(baseDir, year).kbIdToWikiTypeMap
     val wikiStructuredTypeMap = KBPQuery.getHelper(baseDir, year).kbIdToWikiStructuredTypeMap
     val wikiStructuredType = wikiStructuredTypeMap.get(kbLink).getOrElse("")
     val wikiType = wikiTypeMap.get(kbLink).getOrElse("").toLowerCase()
     if(wikiType.contains("settlement") || wikiType.contains("city") || wikiType.contains("place") || wikiType.contains("province")
         || wikiType.contains("country") || wikiType.contains("county") || 
         wikiStructuredType == "GPE"){
       return true
     }
     false
  }
  
  def isSportsTeam(kbLink: String) : Boolean = {
    val wikiTypeMap = KBPQuery.getHelper(baseDir,year).kbIdToWikiTypeMap
    val wikiType = wikiTypeMap.get(kbLink).getOrElse("").toLowerCase()
    if(wikiType.contains("team") || wikiType.contains("club")){
      return true
    }
    false
  }
}