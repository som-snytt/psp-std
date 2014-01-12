seq(Revolver.settings: _*)

javaOptions in Revolver.reStart += "-Xmx2g"

mainClass in Revolver.reStart := Some("com.example.Main")

Revolver.reStartArgs := Seq[String]()

name := "psp-view"

description := "psp-view description"

organization := "org.improving"

homepage := Some(url("https://github.com/paulp/psp-view"))

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.10.3"

fork in run := true // without this JNI breaks, type tags break

parallelExecution in Test := false

licenses := Seq("Apache" -> url("http://www.apache.org/licenses/LICENSE-2.0"))

def projectString(s: State): String = (
  (Project extract s).currentRef.project.toString match {
    case s if s startsWith "default-" => ""
    case s                            => "#" + s
  }
)

shellPrompt := (s => name.value + projectString(s) + "> ")

initialCommands in console := """
import scala.collection.mutable
import scala.reflect.runtime.{ universe => ru }
import improving._
"""

logBuffered := false

libraryDependencies ++= Seq(
  // "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  // "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  // "org.specs2" %% "specs2" % "2.0 % "test",
  // "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"
)

scalacOptions ++= Seq(
  // "-Xlint", "-Xdev"
)

// resolvers += Opts.resolver.sonatypeSnapshots

// retrieveManaged := true
