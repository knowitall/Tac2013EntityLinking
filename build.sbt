name := "Tac2013-EntityLinking"

version := "1.0"

scalaVersion := "2.10.1"

libraryDependencies ++= Seq(
  "com.github.scopt" %% "scopt" % "2.1.0",
  "edu.washington.cs.knowitall.openie" %% "openie-models" % "1.0",
  "edu.washington.cs.knowitall.openie" %% "openie-linker" % "1.0",
  "edu.washington.cs.knowitall.nlptools" %% "nlptools-sentence-opennlp" % "2.4.2",
  "edu.stanford.nlp" % "stanford-corenlp" % "1.3.5",
  "jp.sf.amateras.solr.scala" %% "solr-scala-client" % "0.0.8"
)

resolvers ++= Seq(
  "nicta" at "http://nicta.github.com/scoobi/releases",
  "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/",
  "amateras-repo" at "http://amateras.sourceforge.jp/mvn/"
)

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource

