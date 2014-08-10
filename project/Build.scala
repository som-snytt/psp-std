package psp
package build

import sbt._, Keys._
import bintray.Plugin.bintraySettings
import com.typesafe.tools.mima.plugin.MimaKeys._
import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings

object Build extends sbt.Build {
  def pspOrg = "org.improving"
  def imports = """
    import java.{ lang => jl, util => ju }
    import java.nio.{ file => jnf }
    import psp.std._
  """.trim

  def isRelease   = sys.props contains "psp.release"
  def isMilestone = sys.props contains "psp.milestone"
  def milestone   = 2

  def releaseVersion   = "0.1.0"
  def milestoneVersion = "%s-M%s".format(releaseVersion, milestone)
  def snapshotVersion  = milestoneVersion + "-SNAPSHOT"
  def buildVersion     = if (isRelease) releaseVersion else if (isMilestone) milestoneVersion else snapshotVersion

  def common = bintraySettings ++ Seq[Setting[_]](
               organization :=  pspOrg,
                    version :=  buildVersion,
               scalaVersion :=  "2.11.2",
                logBuffered :=  false,
              scalacOptions ++= Seq("-language:_"),
               javacOptions ++= Seq("-nowarn", "-XDignore.symbol.file"),
                   licenses :=  Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    publishArtifact in Test :=  false
  )

  /** Run 'sbt mima'.
   */
  val mima = taskKey[Unit]("run binary compatibility test")

  // Mima won't resolve the %% cross version.
  private def lastRelease = pspOrg % "psp-std_2.11" % "0.1.0-M1"

  /** What is accomplished by this structure?
   *
   *   - std does not depend on scala-reflect
   *   - the macro project can make use of psp.std
   *   - when testing psp.std, we have access to psp.macros
   *   - after touching a testfile, the macro runs and the tests run, but the entire project isn't rebuilt
   *
   *  It's harder to get all those at once than you may think.
   */
  lazy val root = project in file(".") dependsOn (macros, std) aggregate (macros, std) settings (common: _*) settings (
                           name :=  "psp-std-root",
                    description :=  "psp's project which exists to please sbt",
                    shellPrompt :=  (s => "%s#%s> ".format(name.value, (Project extract s).currentRef.project)),
     initialCommands in console :=  imports,
                        publish :=  (),
                   publishLocal :=  (),
                publishArtifact :=  false,
                           mima <<= reportBinaryIssues in std,
                           test <<= run in Test toTask "" dependsOn (clean in Test)
  )

  lazy val std = project settings (mimaDefaultSettings ++ common: _*) settings (
                name := "psp-std",
         description := "psp's non-standard standard library",
    previousArtifact := Some(lastRelease)
  )

  lazy val macros = project dependsOn std settings (common: _*) settings (
                   name := "psp-std-macros",
            description := "macros for psp's non-standard standard library",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )
}
