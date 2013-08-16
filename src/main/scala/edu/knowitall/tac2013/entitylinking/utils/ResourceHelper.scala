package edu.knowitall.tac2013.entitylinking.utils

import edu.knowitall.tac2013.entitylinking.coref.SerializeCorefOffsetsData
import edu.knowitall.tac2013.entitylinking.coref.SerializeNamedEntities

object ResourceHelper {
  
  def initialize(year: String){
    //check that corefMentions file exists, if not create it...
    try{
      	  val corefMentionsFile = getClass.getResource("/edu/knowitall/tac2013/entitylinking/coref/"+year+"corefmentions.txt").getPath()
    }
    catch{
      //file is not in location, so create it
      case e: Exception =>{
    	  SerializeCorefOffsetsData.serializeCorefOffsetsData(year)
      }
    }
    
    
    //check that namedEntityMentions file exists, if not create it...
    try{
      	  val namedEntityMentionsFile = getClass.getResource("/edu/knowitall/tac2013/entitylinking/coref/"+year+"namedEntities.txt").getPath()
    }
    catch{
      //file is not in location, so create it
      case e: Exception =>{
    	  SerializeNamedEntities.serializeNamedEntities(year)
      }
    }
  }

}