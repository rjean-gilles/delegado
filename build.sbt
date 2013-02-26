resolvers ++= Seq(
  "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/",
  "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
)

resolvers ++= Seq(
  "Typesafe Releases Repository" at "http://repo.typesafe.com/typesafe/releases/",
  "Typesafe Snapshots Repository" at "http://repo.typesafe.com/typesafe/snapshots/"
)

scalaVersion := "2.10.0"

version := "0.1"

libraryDependencies <+= scalaVersion( "org.scala-lang" % "scala-reflect" % _ )

libraryDependencies += "org.specs2" %% "specs2" % "1.14" % "test"

scalacOptions ++= List(/*"-Ymacro-debug-lite"*/)
