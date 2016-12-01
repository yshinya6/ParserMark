name := "epsilon"

version := "1.0"

scalaVersion := "2.12.0"

unmanagedBase := baseDirectory.value / "libs"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
)