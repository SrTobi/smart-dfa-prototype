name := "dfa-test"

version := "0.1"

scalaVersion := "2.13.1"

scalacOptions := Seq("-unchecked", "-deprecation")

libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % "test"
libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.2.2"