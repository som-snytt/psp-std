package psp
package build

import sbt._, Keys._
import bintray.Plugin.bintraySettings
import com.typesafe.tools.mima.plugin.MimaKeys._
import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings
import psp.meta._

object Build extends sbt.Build with Versioning {
  // scala.reflect.runtime.currentMirror.staticPackage("psp.core")
  def opts = if (sys.props contains "debug") Seq("-Ylog:all") else Nil

  def imports = """
    import scala.collection.{ mutable, immutable }
    import psp.std._, psp.core._, ansi._
    implicit final class ReplForeachOps[A](val target: Foreach[A]) {
      def !> : Unit = println(target mkString EOL)
      def  >(implicit shows: Show[A]): Unit = println(target join EOL)
    }
    implicit final class ReplOps[A](val target: A) {
      def !> : Unit = println(target)
      def >(implicit shows: Show[A]): Unit = println(target.to_s)
    }
  """

  def inAllProjects[T](projects: => Seq[ProjectReference], key: SettingKey[T]): Project.Initialize[Seq[T]] = Def settingDyn {
    val lb = loadedBuild.value
    val pr = thisProjectRef.value
    def resolve(ref: ProjectReference): ProjectRef = Scope.resolveProjectRef(pr.build, Load.getRootProject(lb.units), ref)
    val refs = projects flatMap { base => Defaults.transitiveDependencies(resolve(base.project), lb, includeRoot = true, classpath = true, aggregate = true) }
    refs map (ref => (key in ref).?) joinWith (_ flatMap { x => x })
  }

  def deepTasks[T](scoped: TaskKey[Seq[T]]): TaskOf[Seq[T]] =
    deep(scoped.task)(_.join map (_.flatten.distinct))

  def deep[T](scoped: SettingKey[T]): SettingOf[Seq[T]] =
    inAllProjects(subprojects map (p => LocalProject(p.id)), scoped)

  import Sxr._
  def fullSxrSettings: SettingSeq = Sxr.settings ++ Seq(
      libraryDependencies +=  "org.improving" %% "sxr" % "1.0.0" % SxrConfig.name,
           sources in sxr <<= deepTasks(sources in Compile),
     sxrSourceDirectories <<= deep(sourceDirectories in Compile).map(_.flatten), // to properly relativize the source paths
     fullClasspath in sxr <<= deepTasks(externalDependencyClasspath in Compile)
  )
  def common = bintraySettings ++ mimaDefaultSettings ++ Seq[Setting[_]](
                    resolvers +=  "bintray/paulp" at "https://dl.bintray.com/paulp/maven",
                 organization :=  pspOrg,
                 scalaVersion :=  "2.11.2",
                      version :=  "0.2.0-M2",
                  logBuffered :=  false,
                scalacOptions ++= (Seq("-language:_") ++ opts),
                 javacOptions ++= Seq("-nowarn", "-XDignore.symbol.file"),
                     licenses :=  Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
      publishArtifact in Test :=  false,
    parallelExecution in Test :=  false,
                 fork in Test :=  true
  )

  def subprojects = List(std, macros, view)

  /** mima command optionally takes a baseline version, e.g. sbt 'mima 0.1.0-M1'
   */
  def mimaCommand = stateCommand {
    case (s, Nil)            => mimaRun(s, stdArtifact("0.2.0-M2"))
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
  lazy val root = project in file(".") dependsOn (std, macros, view) aggregate (std, macros, view) settings (common: _*) settings (
                          name :=  "psp-std-root",
                   description :=  "psp's project which exists to please sbt",
                   shellPrompt :=  (s => "%s#%s> ".format(name.value, (Project extract s).currentRef.project)),
    initialCommands in console :=  imports,
          aggregate in publish :=  false,
     aggregate in publishLocal :=  false,
               publishArtifact :=  false,
                       publish <<= runPublish(publish),
                  publishLocal <<= runPublish(publishLocal),
                      commands +=  Command.args("mima", "<version>")(mimaCommand),
                      commands +=  Command.command("ccon")(s => s set (libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value) runTask (console in Compile) _1),
           testOptions in Test +=  Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "1"),
           libraryDependencies +=  "org.scalacheck" %% "scalacheck" % "1.11.5" % "test",
             mainClass in Test :=  Some("psp.tests.TestRunner"),
                          test <<= run in Test toTask "" dependsOn (Keys.`package` in Compile) dependsOn (clean in Test)
  )

  implicit class ProjectOps(val p: Project) {
    def sxr: Project                  = p configs SxrConfig settings (fullSxrSettings: _*)
    def also(ss: SettingSeq): Project = p settings (ss: _*)
  }

  lazy val std = project.sxr also common settings (
            name := "psp-std",
     description := "psp's non-standard standard library"
  )

  lazy val macros = project dependsOn std also common settings (
                   name := "psp-std-macros",
            description := "macros for psp's non-standard standard library",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )

  lazy val view = project.sxr dependsOn std also common settings (
            name :=  "psp-view",
     description :=  "collections for psp's non-standard standard library"
  )
}

trait Versioning {
  import scala.sys.process._

  def subprojects: List[Project]

  // It's a pretty hideous version string, but it should guarantee we never overwrite
  // artifacts and that dependency managers will see the local version numbers as increasing.
  def pspOrg           = "org.improving"
  def releaseProp      = "psp.release"
  def hasReleaseProp   = sys.props contains releaseProp
  def isRepoClean      = "git diff --quiet --exit-code HEAD".! == 0
  def repoSha: String  = "git rev-parse HEAD".!! take 10
  def repoDiff: String = ("git diff" #| "md5").!! take 10 // not using at present - hopefully "dirty" is enough
  def localSuffix      = "-%s-%s%s".format(dateTime, repoSha, if (isRepoClean) "" else "-dirty")

  // TODO - Distinguish between milestones and releases.
  def runPublish[A](key: TaskKey[A]) = Def.task[Unit] {
    val state = Keys.state.value
    def isPublishKey               = key eq publish
    def versionString              = state(version) + ( if (isPublishKey) "" else localSuffix )
    def publishProject(p: Project) = state.put(version in p, versionString) runTask (key in Compile in p)
    def finish()                   = subprojects foreach publishProject

    // TODO - Require the git tag to exist (not for milestones) or create it.
    if (!isPublishKey)
      finish()
    else if (!isRepoClean)
      state err "Can't publish with a dirty repository."
    else if (!hasReleaseProp)
      state err s"As a safeguard, publishing release artifacts requires -D$releaseProp."
    else
      finish()
  }

  // Mima won't resolve the %% cross version.
  def stdArtifact(version: String): ModuleID = pspOrg % "psp-std_2.11" % version
}
