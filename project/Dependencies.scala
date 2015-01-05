import sbt._

object Dependencies {
  import Settings.metaVersion

  def reflect(sv: String) = "org.scala-lang" % "scala-reflect" % sv
  def compiler(sv: String) = "org.scala-lang" % "scala-compiler" % sv
  lazy val scalatest = "org.scalatest" %% "scalatest" % "2.1.3" % "test"
  lazy val scalacheck = "org.scalacheck" %% "scalacheck" % "1.11.3" % "test"

  lazy val scalameta = "org.scalameta" % "scalameta" % metaVersion cross CrossVersion.binary
  lazy val scalahost = "org.scalameta" % "scalahost" % metaVersion cross CrossVersion.full

  lazy val kiama2S   = "com.googlecode.kiama" %% "kiama" % "2.0.0-SNAPSHOT"

  lazy val sgraph        = "com.assembla.scala-incubator" %% "graph-core"         % "1.9.1"
  lazy val sgConstrained = "com.assembla.scala-incubator" %% "graph-constrained"  % "1.9.0"
  lazy val sgDot         = "com.assembla.scala-incubator" %% "graph-dot"          % "1.10.0"
  lazy val sgJSON        = "com.assembla.scala-incubator" %% "graph-json"         % "1.10.0"
  lazy val sgTest        = "com.assembla.scala-incubator" %% "graph-test"         % "1.9.2"
}
