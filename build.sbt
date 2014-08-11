resolvers += "bintray/paulp" at "https://dl.bintray.com/paulp/maven"

scalaVersion in Global := "2.11.2"

organization in Global := "org.improving"

version in Global := "0.2.0-M2"

exportJars in Global := true

initialCommands in console := """
import scala.collection.{ mutable, immutable }
import scala.reflect.runtime.{ universe => ru }
import psp.core._
implicit final class ReplForeachOps[A](val target: Foreach[A]) {
  def >(implicit shows: Show[A]): Unit = println(target join EOL)
}
implicit final class ReplOps[A](val target: A) {
  def >(implicit shows: Show[A]): Unit = println(target.to_s)
}
"""

libraryDependencies ++= Seq(
  organization.value %% "psp-std"        %     "0.1.0-M4",
  "org.scala-lang"    % "scala-compiler" % scalaVersion.value,
  "org.scalacheck"   %% "scalacheck"     %      "1.11.5"       % "test",
  "com.lihaoyi"      %% "utest"          %       "0.2.0"       % "test"
)

lazy val root = project in file(".") settings (
  name                      := "psp-view",
  description               := "psp alternate view implementation",
  homepage                  := Some(url("https://github.com/paulp/psp-view")),
  licenses                  := Seq("Apache" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  shellPrompt               := (s => name.value + projectString(s) + "> "),
  parallelExecution in Test := false,
  fork in Test              := true
)

testFrameworks += new TestFramework("utest.runner.JvmFramework")

def projectString(s: State): String = (
  (Project extract s).currentRef.project.toString match {
    case s if s startsWith "default-" => ""
    case s                            => "#" + s
  }
)
