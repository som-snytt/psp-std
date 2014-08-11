package psp
package build

import sbt._, Keys._
import bintray.Plugin.bintraySettings
import com.typesafe.tools.mima.plugin.MimaKeys._
import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings

object Build extends sbt.Build with Versioning {
  def targetReleaseVersion = "0.1.0"

  def common = bintraySettings ++ Seq[Setting[_]](
               organization :=  pspOrg,
               scalaVersion :=  "2.11.2",
                    version :=  "0.1.0",
                logBuffered :=  false,
              scalacOptions ++= Seq("-language:_"),
               javacOptions ++= Seq("-nowarn", "-XDignore.symbol.file"),
                   licenses :=  Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    publishArtifact in Test :=  false
  )

  def subprojects = List(std, macros)

  /** mima command optionally takes a baseline version, e.g. sbt 'mima 0.1.0-M1'
   */
  def mimaCommand = stateCommand {
    case (s, Nil)            => mimaRun(s, stdArtifact("0.1.0-M3"))
    case (s, version :: Nil) => mimaRun(s, stdArtifact(version))
    case (s, _)              => s.fail
  }

  private def mimaRun(state: State, module: ModuleID): Unit =
    state.put(previousArtifact in std, Some(module)) runTask (reportBinaryIssues in std)

  /** What is accomplished by this structure?
   *
   *   - std does not depend on scala-reflect
   *   - the macro project can make use of psp.std
   *   - when testing psp.std, we have access to psp.macros
   *   - after touching a testfile, the macro runs and the tests run, but the entire project isn't rebuilt
   *
   *  It's harder to get all those at once than you may think.
   */
  lazy val root = project in file(".") dependsOn (std, macros) aggregate (std, macros) settings (common: _*) settings (
                          name :=  "psp-std-root",
                   description :=  "psp's project which exists to please sbt",
                   shellPrompt :=  (s => "%s#%s> ".format(name.value, (Project extract s).currentRef.project)),
    initialCommands in console :=  "import psp.std._",
          aggregate in publish :=  false,
     aggregate in publishLocal :=  false,
               publishArtifact :=  false,
                       publish <<= Def task runPublish(subprojects, state.value),
                  publishLocal <<= Def task runPublishLocal(subprojects, state.value),
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


trait Versioning {
  def pspOrg             = "org.improving"
  def isRepoClean        = Process("git diff --quiet --exit-code HEAD").! == 0
  def currentSha: String = Process("git rev-parse HEAD").lines.mkString take 10

  def runPublish(projects: Seq[Project], state: State): Unit = (
    if (!isRepoClean)
      state err "Can't publish with a dirty repository."
    else if (!isRelease)
      state err "As a safeguard, publishing release artifacts requires -Dpsp.release."
    else
      runPublishes(projects, state, publish, state(version))
  )
  def runPublishLocal(projects: Seq[Project], state: State): Unit = {
    def suffix = "-%s%s".format(currentSha, if (isRepoClean) "" else "-dirty")
    runPublishes(projects, state, publishLocal, state(version) + suffix)
  }

  private def runPublishes(projects: Seq[Project], state: State, key: TaskKey[Unit], versionString: String): Unit =
    projects foreach (p => state.put(version in p, versionString) runTask (key in Compile in p))

  // Mima won't resolve the %% cross version.
  def stdArtifact(version: String): ModuleID = pspOrg % "psp-std_2.11" % version
}
