package edu.knowitall.tac2013.entitylinking.classifier

import edu.knowitall.tool.conf.Feature
import edu.knowitall.tool.conf.FeatureSet
import edu.knowitall.browser.entity.EntityLink
import scala.collection.immutable.SortedMap


object LinkFeatures {

  type LinkFeature = Feature[EntityLink, Double]

  case object candidateScore extends LinkFeature("candidate score") {
    def apply(link: EntityLink) = {
      link.candidateScore
    }
  }

  object inlinkScore extends LinkFeature("inlink score") {
    def apply(link: EntityLink) = {
      math.log(link.inlinks)
    }
  }

  object docSimScore extends LinkFeature("docsim score") {
    def apply(link: EntityLink) = {
      link.docSimScore
    }
  }

  private val features = Seq(candidateScore, inlinkScore, docSimScore)
  
  def featureSet = new FeatureSet(SortedMap.empty[String, Feature[EntityLink, Double]] ++ features.map(f => (f.name, f)).toMap) 
}