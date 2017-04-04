organization := "org.quantlib"

name := "quantlib-numeric"

version := "1.0"

scalaVersion := "2.12.0"

lazy val math1d = Seq("org.apache.commons" % "commons-math3" % "3.6.1")

lazy val scalaTest = Seq("org.scalatest" %% "scalatest" % "3.0.0" % "test")

libraryDependencies ++= scalaTest ++ math1d
