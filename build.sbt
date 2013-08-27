name := "Tac2013-EntityLinking"

version := "1.0"

scalaVersion := "2.10.1"

javaOptions += "-Xmx12G"

fork in run := true

libraryDependencies ++= Seq(
  "com.github.scopt" %% "scopt" % "2.1.0",
  "edu.washington.cs.knowitall.taggers" %% "taggers" % "0.2",
  "edu.washington.cs.knowitall.openie" %% "openie-linker" % "1.1-SNAPSHOT",
  "edu.washington.cs.knowitall.nlptools" %% "nlptools-sentence-opennlp" % "2.4.2",
  "edu.stanford.nlp" % "stanford-corenlp" % "1.3.5",
  "edu.washington.cs.knowitall.stanford-corenlp" % "stanford-ner-models" % "1.3.5",
  "edu.washington.cs.knowitall.stanford-corenlp" % "stanford-postag-models" % "1.3.5",
  "edu.washington.cs.knowitall.stanford-corenlp" % "stanford-dcoref-models" % "1.3.5",
  "edu.washington.cs.knowitall.stanford-corenlp" % "stanford-parse-models" % "1.3.5",
  "jp.sf.amateras.solr.scala" %% "solr-scala-client" % "0.0.8",
  "edu.washington.cs.knowitall.nlptools" %% "nlptools-conf-breeze" % "2.4.2",
  "edu.washington.cs.knowitall.nlptools" %% "nlptools-postag-clear" % "2.4.2",
  "com.rockymadden.stringmetric" % "stringmetric-core" % "0.25.3",
  "edu.knowitall" %% "slotfiller" % "0.0.1"
)

resolvers ++= Seq(
  "nicta" at "http://nicta.github.com/scoobi/releases",
  "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/",
  "amateras-repo" at "http://amateras.sourceforge.jp/mvn/",
  "kbp2013" at "file:///projects/WebWare6/KBP_2013/mavenrepo/"
)

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource

