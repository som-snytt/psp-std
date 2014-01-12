name := "psp-core"

description  := "psp alternate collections, core"

parallelExecution in Test := false

fork in Test := true

libraryDependencies ++= Seq(
  "org.specs2" % "specs2-scalacheck_2.11.0-M7" % "2.3.4" % "test" //intransitive()
  // "org.specs2" %% "specs2-scalacheck" % "2.3.4" % "test"
)

