organization := "org.quantlib"

name := "quantlib-numeric"

version := "1.0"

scalaVersion := "2.11.8"

lazy val math1d = Seq("org.apache.commons" % "commons-math3" % "3.6.1")

lazy val scalaTest = Seq("org.scalatest" %% "scalatest" % "3.0.0" % "test")


lazy val nd4s = {
  val nd4jVersion = "0.8.0"
  Seq(
    "org.nd4j" % "nd4j-native-platform" % nd4jVersion,
    "org.nd4j" %% "nd4s" % nd4jVersion
  )
}

libraryDependencies ++= scalaTest ++ math1d ++ nd4s
