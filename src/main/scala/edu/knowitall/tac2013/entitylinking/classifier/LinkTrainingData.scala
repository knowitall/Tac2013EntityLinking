package edu.knowitall.tac2013.entitylinking.classifier

import edu.knowitall.tool.conf.Labelled
import edu.knowitall.browser.entity._
import edu.knowitall.tac2013.entitylinking.KBPQuery

class LinkTrainingData(val baseDir: String = "/scratch", val year: String = "2012") extends Iterable[Labelled[KBPQueryLink]] {
  
  val linkerSupportPath = new java.io.File(baseDir)
  val linker = new EntityLinker(
    new batch_match(linkerSupportPath),
    new CrosswikisCandidateFinder(linkerSupportPath, 0.00, 1),
    new EntityTyper(linkerSupportPath))
  
  val trainingResource = {
    val name = s"linker-classifier-training-$year.csv"
    val url = getClass.getResource(name)
    require(url != null, s"Could not find $url")
    url
  }
  
  def lineToLink(line: String): Option[Labelled[KBPQueryLink]] = {
    
    val split = line.split("\t")
    split match {
      case Array(label, qid, used, expected, _*) => {
        val query = qMap(qid)
        val context = query.corefSourceContext.mkString(" ")
        fieldsToLink(query, label.trim, used, expected, context)
      }
      case _ => {
        System.err.println(s"Malformed training line $line")
        None
      }
    }
  }
  
  val splitRegex = "\\.\\s[A-Z]".r
  
  def fieldsToLink(query: KBPQuery, label: String, entity: String, expected: String, context: String): Option[Labelled[KBPQueryLink]] = {
    
    val linksMap = linker.getBestEntities(entity, splitRegex.split(context)).map(l => (l.entity.name, l)).toMap
    linksMap.get(expected) match {
      case Some(link) => {
        query.entityString = entity
        Some(Labelled(label == "CORRECT", new KBPQueryLink(query, link)))
      }
      case None => {
        System.err.println(s"Warning, didn't find link $expected for string $entity")
        None
      }
    }
  }
  
  val kbpQueryHelper = KBPQuery.getHelper(baseDir,year)

  val queries = kbpQueryHelper.parseKBPQueries(getClass.getResource("/edu/knowitall/tac2013/entitylinking/tac_"+year+"_kbp_english_evaluation_entity_linking_queries.xml").getPath()).toSeq
  val qMap = queries.map(q => (q.id, q)).toMap
  
  val kbpQueryLinks = io.Source.fromURL(trainingResource, "UTF8").getLines.flatMap(lineToLink).toSeq
  
  def iterator = kbpQueryLinks.iterator
}