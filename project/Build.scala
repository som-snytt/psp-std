package psp
package build

import sbt._, Keys._, psp.libsbt._
import bintray.Plugin._, bintray.Keys._

object Build extends sbt.Build with LibSbt {
  def rootResourceDir: SettingOf[File] = resourceDirectory in Compile in LocalRootProject

  private def commonSettings = bintraySettings ++ standardSettings ++ Seq(
    bintrayOrganization in bintray :=  None,
             repository in bintray :=  "maven",
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

  lazy val root = setup(project.root, "root aggregator") aggregate (api, std) dependsOn (api, std) settings (
         console <<= console in consoleOnly,
            test <<= test in testOnly,
    watchSources ++= (sources in Test in testOnly).value
  )
  lazy val api  = setup(project, "api for psp's non-standard standard library")
  lazy val std  = setup(project, "psp's non-standard standard library") dependsOn api

  lazy val testOnly = setup(project.noArtifacts, "test encapsulation") dependsOn (api, std) settings (
          testOptions in Test  +=  Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "1"),
    parallelExecution in Test  :=  false,
                 fork in Test  :=  true,
                  logBuffered  :=  false,
          libraryDependencies <++= testDependencies,
    sourceDirectories in Test  :=  Seq(baseDirectory.value / "src/test/scala", baseDirectory.value / s"src/test/scala_${scalaBinaryVersion.value}"),
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
