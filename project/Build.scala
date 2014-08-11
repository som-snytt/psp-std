package psp
package build

import sbt._, Keys._
import bintray.Plugin.bintraySettings
import com.typesafe.tools.mima.plugin.MimaKeys._
import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings

object Build extends sbt.Build {
  def mimaRun(state: State, module: ModuleID): Unit =
    state.put(previousArtifact in std, Some(module)) get (reportBinaryIssues in std)

  /** mima command optionally takes a baseline version, e.g. sbt 'mima 0.1.0-M1'
   */
  def mimaCommand = stateCommand {
    case (s, Nil)            => mimaRun(s, lastReleasedArtifact)
    case (s, version :: Nil) => mimaRun(s, stdArtifact(version))
    case (s, _)              => s.fail
  }

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
     initialCommands in console :=  "import psp.std._",
                        publish :=  (),
                   publishLocal :=  (),
                publishArtifact :=  false,
                       commands +=  Command.args("mima", "<version>")(mimaCommand),
                           test <<= run in Test toTask "" dependsOn (clean in Test)
  )

  lazy val std = project settings (mimaDefaultSettings ++ common: _*) settings (
            name := "psp-std",
     description := "psp's non-standard standard library"
  )

  lazy val macros = project dependsOn std settings (common: _*) settings (
                   name := "psp-std-macros",
            description := "macros for psp's non-standard standard library",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )
}
