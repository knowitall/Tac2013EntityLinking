package edu.knowitall.tac2013.entitylinking.classifier

import edu.knowitall.tool.conf.Labelled
import edu.knowitall.browser.entity._

class LinkTrainingData(val baseDir: String = "/scratch/") extends Iterable[Labelled[EntityLink]] {
  
  val linkerSupportPath = new java.io.File(baseDir)
  val linker = new EntityLinker(
    new batch_match(linkerSupportPath),
    new CrosswikisCandidateFinder(linkerSupportPath, 0.01, 10),
    new EntityTyper(linkerSupportPath))
  
  val trainingResource = {
    val name = "linker-classifier-training.csv"
    val url = getClass.getResource(name)
    require(url != null, s"Could not find $url")
    url
  }
  
  def lineToLink(line: String): Option[Labelled[EntityLink]] = {
    
    val split = line.split("\t")
    split match {
      case Array(label, entity, expected, context, _*) => fieldsToLink(label, entity, expected, context)
      case _ => throw new RuntimeException(s"Malformed training line $line")
    }
  }
  
  val splitRegex = "\\.\\s[A-Z]".r
  
  def fieldsToLink(label: String, entity: String, expected: String, context: String): Option[Labelled[EntityLink]] = {
    
    val linksMap = linker.getBestEntity(entity, splitRegex.split(context)).map(l => (l.entity.name, l)).toMap
    linksMap.get(expected) match {
      case Some(link) => Some(Labelled(label == "1", link))
      case None => {
        System.err.println(s"Warning, didn't find link $expected for string $entity")
        None
      }
    }
  }
  
  def iterator = new Iterator[Labelled[EntityLink]]() {
    val source = io.Source.fromURL(trainingResource, "UTF8")
    val links = source.getLines.flatMap(lineToLink)
    var closed = false
    def hasNext() = {
      if (closed) false
      else if (!links.hasNext) {
        closed = true
        source.close()
        false
      } else {
        true
      }
    }
    def next = {
      links.next
    }
  }
}