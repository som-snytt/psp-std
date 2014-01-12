import AssemblyKeys._

assemblySettings

resolvers in Global += Resolver.mavenLocal

resolvers in Global += Opts.resolver.sonatypeSnapshots

scalaBinaryVersion in Global := "2.11.0-M7"

retrieveManaged := true

scalaVersion in Global := "2.11.0-SNAPSHOT"

// scalaHome in Global := Some(file("/scala/inst/scala-2.11.0-20140111-120659-9c95939865"))

organization in Global := "org.improving"

version in Global := "0.1.0-SNAPSHOT"

exportJars in Global := true

initialCommands in console := s"cat ${baseDirectory.value}/src/main/resources/replStartup.scala".!!

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  "jline" % "jline" % "2.11"
)

lazy val common, demo = project

lazy val core = project dependsOn common

lazy val root = project in file(".") dependsOn (common, core, demo) aggregate (common, core, demo) settings (assemblySettings: _*) settings (
  name                   := "psp-view",
  description            := "psp alternate view implementation",
  homepage               := Some(url("https://github.com/paulp/psp-view")),
  licenses               := Seq("Apache" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  shellPrompt            := (s => name.value + projectString(s) + "> "),
  target in assembly     := baseDirectory.value,
  mainClass in assembly  := Some("psp.repl.Main"),
  jarName in assembly    := "psp.jar"
)

def projectString(s: State): String = (
  (Project extract s).currentRef.project.toString match {
    case s if s startsWith "default-" => ""
    case s                            => "#" + s
  }
)
