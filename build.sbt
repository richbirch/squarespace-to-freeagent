import sbt.Keys.libraryDependencies
//name := "squarespace-to-freeagent"
//
//version := "0.1"
//
//scalaVersion := "2.12.6"

lazy val root = (project in file("."))
  .settings(
    name := "squarespace-to-freeagent",
    scalaVersion := "2.12.6",
    libraryDependencies += "org.specs2" %% "specs2-core" % "4.1.0" % "test",
    libraryDependencies += "joda-time" % "joda-time" % "2.9.9",
    libraryDependencies += "io.circe" %% "circe-core" % "0.9.3",
    libraryDependencies += "io.circe" %% "circe-generic" % "0.9.3",
    libraryDependencies += "io.circe" %% "circe-parser" % "0.9.3",
    libraryDependencies += "com.typesafe" % "config" % "1.3.0",
    libraryDependencies += "com.typesafe.akka" %% "akka-http" % "10.1.1",
    libraryDependencies += "com.typesafe.akka" %% "akka-stream" % "2.5.11"
  )
