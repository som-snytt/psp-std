package psp
package build

import sbt._, Keys._, psp.libsbt._

object Build extends sbt.Build with LibSbt {
  def is211: SettingOf[Boolean]        = scalaBinaryVersion mapValue (_ == "2.11")
  def rootResourceDir: SettingOf[File] = resourceDirectory in Compile in LocalRootProject

  private def commonSettings = standardSettings ++ Seq(
                version :=  publishVersion,
           scalaVersion :=  scalaVersionLatest,
     crossScalaVersions :=  scalaVersionsCross,
               licenses :=  pspLicenses,
           organization :=  pspOrg,
          scalacOptions ++= scalacOptionsFor(scalaBinaryVersion.value),
      publishMavenStyle :=  true
  )
  private def setup(p: Project, text: String): Project = p also commonSettings ++ Seq(
           name := "psp-" + p.id,
    description := text
  )

  lazy val root = setup(project.root, "root aggregator") aggregate (api, std) settings (
     console <<= console in consoleOnly,
        test <<= test in testOnly
  )
  lazy val api  = setup(project, "api for psp's non-standard standard library")
  lazy val std  = setup(project, "psp's non-standard standard library") dependsOn api settings (publishArtifact := is211.value)

  lazy val testOnly = setup(project.noArtifacts, "test encapsulation") dependsOn (api, std) settings (
          testOptions in Test  +=  Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "1"),
    parallelExecution in Test  :=  false,
                 fork in Test  :=  true,
                  logBuffered  :=  false,
          libraryDependencies <++= testDependencies,
            mainClass in Test  :=  Some("psp.tests.TestRunner"),
                         test  :=  (run in Test toTask "").value
  )
  // A console project which pulls in misc additional dependencies currently being explored.
  // Removing all scalac options except the ones listed here, to eliminate all the warnings
  // repl startup code in resources/console.scala
  lazy val consoleOnly = setup(project.noArtifacts, "console encapsulation") dependsOn (std, api) settings (
                     libraryDependencies <++= consoleDependencies,
                                 console <<=  console in Compile,
     scalacOptions in (Compile, console)  :=  Seq("-language:_"),
              initialCommands in console <+=  rootResourceDir mapValue (d => IO.read(d / "console.scala"))
  )

  def testDependencies = Def setting Seq(
    "org.scala-lang"  % "scala-reflect" % scalaVersion.value,
    "org.scalacheck" %% "scalacheck"    %      "1.11.5"       % "test"
  )
  def consoleDependencies = Def setting Seq(
    "org.scala-lang"            % "scala-compiler" % scalaVersion.value,
    "org.spire-math"           %% "spire"          %      "0.8.2",
    "com.chuusai"              %% "shapeless"      %      "2.0.0",
    "com.google.guava"          % "guava"          %       "17.0",
    "net.sourceforge.findbugs"  % "jsr305"         %       "1.3.7"
  )
}
