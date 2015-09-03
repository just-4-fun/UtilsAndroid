
lazy val utils = project.in(file("."))
  .settings(commonSettings: _*)
  .settings(testSettings: _*)
  .settings(paradiseSettings: _*)
  .settings(dependencies)

lazy val commonSettings = Seq(
	name := "utils"
	, organization := "just4fun"
	, version := "1.0-SNAPSHOT"
	, scalaVersion := "2.11.6"
	, exportJars := true
	, licenses := Seq("BSD-style" -> url("http://www.opensource.org/licenses/bsd-license.php"))
	, homepage := Some(url("https://github.com/just-4-fun"))
)

lazy val testSettings = Seq(
	publishArtifact in Test := false
	, libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
)

lazy val paradiseSettings = Seq(
	resolvers += Resolver.sonatypeRepo("snapshots")
	, resolvers += Resolver.sonatypeRepo("releases")
	, addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)
	, libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)
)

lazy val dependencies = Seq(
	libraryDependencies += "com.fasterxml.jackson.core" % "jackson-core" % "2.5.1"
)

// link this project via:
// libraryDependencies += "just4fun" %% "utils" % "1.0-SNAPSHOT"

// to publish locally: publishLocal
// run update in linking projects

