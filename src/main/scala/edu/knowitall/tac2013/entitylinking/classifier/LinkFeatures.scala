package edu.knowitall.tac2013.entitylinking.classifier

import edu.knowitall.tool.conf.Feature
import edu.knowitall.tool.conf.FeatureSet
import edu.knowitall.browser.entity.EntityLink
import scala.collection.immutable.SortedMap
import edu.knowitall.tac2013.entitylinking.KBPQuery

class KBPQueryLink(val query: KBPQuery, val link: EntityLink)

object LinkFeatures {

  type LinkFeature = Feature[KBPQueryLink, Double]

  case object candidateScore extends LinkFeature("candidate score") {
    def apply(link: KBPQueryLink) = {
      link.link.candidateScore
    }
  }

  object inlinkScore extends LinkFeature("inlink score") {
    def apply(link: KBPQueryLink) = {
      math.log(link.link.inlinks)
    }
  }

  object docSimScore extends LinkFeature("docsim score") {
    def apply(link: KBPQueryLink) = {
      link.link.docSimScore
    }
  }

  private val features = Seq(candidateScore, inlinkScore, docSimScore)
  
  def featureSet = new FeatureSet(SortedMap.empty[String, Feature[KBPQueryLink, Double]] ++ features.map(f => (f.name, f)).toMap) 
}