import AssemblyKeys._

assemblySettings

scalaVersion in Global := "2.11.0-M8"

organization in Global := "org.improving"

version in Global := "0.1.0-SNAPSHOT"

exportJars in Global := true

initialCommands in console := s"cat ${baseDirectory.value}/src/main/resources/replStartup.scala".!!

libraryDependencies ++= Seq(
  "org.scala-lang"  % "scala-compiler"    % scalaVersion.value,
  "jline"           % "jline"             %       "2.11",
  "ch.qos.logback"  % "logback-classic"   %      "1.0.9",
  "org.specs2"     %% "specs2-scalacheck" %       "2.3.7"       % "test"
)

lazy val utest = ProjectRef(uri("git://github.com/lihaoyi/utest#2.11"), "root") % "test"

lazy val utestRunner = ProjectRef(uri("git://github.com/lihaoyi/utest#2.11"), "runner") % "test"

lazy val root = project in file(".") dependsOn (utest, utestRunner) settings (assemblySettings: _*) settings (
  name                      := "psp-view",
  description               := "psp alternate view implementation",
  homepage                  := Some(url("https://github.com/paulp/psp-view")),
  licenses                  := Seq("Apache" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  shellPrompt               := (s => name.value + projectString(s) + "> "),
  target in assembly        := baseDirectory.value,
  mainClass in assembly     := Some("psp.repl.Main"),
  jarName in assembly       := "psp.jar",
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

scalacOptions ++= Seq(
  // "-Ylog:all"
  // "-Ydebug"
  // "-optimise"
  // "-Xlog-implicit-conversions"
)
