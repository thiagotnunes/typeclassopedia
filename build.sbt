name := "typeclassopedia"

scalaVersion := "2.11.2"

organization := "com.thiago.typeclassopedia"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-language:_")

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "3.6.1" % "test"
)
