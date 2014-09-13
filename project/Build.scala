package psp
package build

import sbt._, Keys._, psp.libsbt._

object Build extends sbt.Build with LibSbt {
  def is211 = Def setting (scalaBinaryVersion.value == "2.11")

  private def commonSettings = standardSettings ++ Seq(
                version :=  publishVersion,
           scalaVersion :=  scalaVersionLatest,
     crossScalaVersions :=  scalaVersionsCross,
               licenses :=  pspLicenses,
           organization :=  pspOrg,
          scalacOptions ++= scalacOptionsFor(scalaBinaryVersion.value),
      publishMavenStyle :=  true
  )

  implicit class ProjectOps(val p: Project) {
    def common: Project            = p settings (commonSettings: _*)
    def sub(text: String): Project = p.common settings (name := "psp-" + p.id, description := text)
    def support: Project           = p.common.noArtifacts
  }

  lazy val root = project.root.common aggregate (api, std) settings (
                          name :=  "psp-std-root",
                   description :=  "psp's project which exists to please sbt",
                       console <<= console in Compile in consoleOnly,
                          test <<= test in testOnly,
    initialCommands in console :=  consoleCode.value
  )
  lazy val api  = project.sub("api for psp's non-standard standard library")
  lazy val std  = project.sub("psp's non-standard standard library") dependsOn api settings (publishArtifact := is211.value)

  lazy val testOnly = project.support dependsOn (api, std) settings (
                         name :=  "psp-test",
                  description :=  "test encapsulation",
          testOptions in Test +=  Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "1"),
    parallelExecution in Test :=  false,
                 fork in Test :=  true,
                  logBuffered :=  false,
          libraryDependencies ++= Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value, "org.scalacheck" %% "scalacheck" % "1.11.5" % "test"),
            mainClass in Test :=  Some("psp.tests.TestRunner"),
                         test :=  (run in Test toTask "").value
  )

  def consoleCode = resourceDirectory in Compile mapValue (d => IO.read(d / "console.scale"))

  def consoleDependencies = Def setting Seq(
    "org.scala-lang"            % "scala-compiler" % scalaVersion.value,
    "org.spire-math"           %% "spire"          %      "0.8.2",
    "com.chuusai"              %% "shapeless"      %      "2.0.0",
    "com.google.guava"          % "guava"          %       "17.0",
    "net.sourceforge.findbugs"  % "jsr305"         %       "1.3.7"
  )

  // A console project which pulls in misc additional dependencies currently being explored.
  lazy val consoleOnly = project.support dependsOn (std, api) settings (
                                   name  :=  "psp-console",
                            description  :=  "console encapsulation",
                    libraryDependencies <++= consoleDependencies,
    scalacOptions in console in Compile  :=  Seq("-language:_")  // turning off all the noisy warnings
             // initialCommands in console  +=  consoleCode.value
  )
}
