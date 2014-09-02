package psp
package build

import sbt._, Keys._
import bintray.Plugin.bintraySettings
import com.typesafe.tools.mima.plugin.MimaKeys._
import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings
import psp.meta._

object Build extends sbt.Build with Versioning with ConsoleOnly with TestOnly {
  // scala.reflect.runtime.currentMirror.staticPackage("psp.core")
  def opts = if (sys.props contains "debug") Seq("-Ylog:all") else Nil

  def ifBinary[A](version: String)(thenp: => A, elsep: => A): TaskOf[A] =
    scalaBinaryVersion map { case `version` => thenp ; case _ => elsep }

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
      libraryDependencies +=  "org.improving" %% "sxr" % "1.0.1" % SxrConfig.name,
           sources in sxr <<= deepTasks(sources in Compile),
     sxrSourceDirectories <<= deep(sourceDirectories in Compile).map(_.flatten), // to properly relativize the source paths
     fullClasspath in sxr <<= deepTasks(externalDependencyClasspath in Compile)
  )
  def common = bintraySettings ++ mimaDefaultSettings ++ Seq[Setting[_]](
                                resolvers  +=  "bintray/paulp" at "https://dl.bintray.com/paulp/maven",
                             organization  :=  pspOrg,
                             scalaVersion  :=  "2.11.2",
                                  version  :=  "0.3.0-M8",
                              logBuffered  :=  false,
                            scalacOptions ++=  opts ++ Seq("-Ywarn-dead-code", "-language:_"),
                            scalacOptions <++= ifBinary("2.11")("-Ywarn-unused" :: "-Ywarn-unused-import" :: Nil, Nil),
                             javacOptions ++=  Seq("-nowarn", "-XDignore.symbol.file"),
                                 licenses  :=  Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
                  publishArtifact in Test  :=  false,
                parallelExecution in Test  :=  false,
                             fork in Test  :=  true,
      scalacOptions in console in Compile  :=  Seq("-language:_"),
    unmanagedSourceDirectories in Compile  +=  (sourceDirectory in Compile).value / s"scala${scalaBinaryVersion.value}",
       unmanagedSourceDirectories in Test  +=  (sourceDirectory in Test).value / s"scala${scalaBinaryVersion.value}"
  )

  def subprojects = List(api, std)

  /** mima command optionally takes a baseline version, e.g. sbt 'mima 0.1.0-M1'
   */
  def mimaCommand = stateCommand {
    case (s, Nil)            => mimaRun(s, stdArtifact("0.3.0-M5"))
    case (s, version :: Nil) => mimaRun(s, stdArtifact(version))
    case (s, _)              => s.fail
  }

  private def mimaRun(state: State, module: ModuleID): Unit =
    state.put(previousArtifact in std, Some(module)) runTask (reportBinaryIssues in std)

  lazy val root = project.root aggregate (api, std) settings (
                           name :=  "psp-std-root",
                    description :=  "psp's project which exists to please sbt",
             crossScalaVersions :=  List("2.10.4", "2.11.2"),
                    shellPrompt :=  (s => "%s#%s> ".format(name.value, (Project extract s).currentRef.project)),
           aggregate in publish :=  false,
      aggregate in publishLocal :=  false,
                publishArtifact :=  false,
                        publish <<= runPublish(publish),
                   publishLocal <<= runPublish(publishLocal),
                       commands +=  Command.args("mima", "<version>")(mimaCommand),
                        console :=  (console in Compile in consoleOnly).value,
                           test :=  (test in testOnly).value
  )

  implicit class ProjectOps(val p: Project) {
    def root: Project                 = p in file(".") also common
    def sub: Project                  = p also common
    def support: Project              = p also common
    def sxr: Project                  = p configs SxrConfig settings (fullSxrSettings: _*)
    def also(ss: SettingSeq): Project = p settings (ss: _*)
  }

  lazy val std = project.sub.sxr dependsOn api settings (
                    name := "psp-std",
             description := "psp's non-standard standard library",
      crossScalaVersions := List("2.11.2")
  )

  lazy val api = project.sub.sxr settings (
                   name := "psp-api",
            description := "api for psp's non-standard standard library",
     crossScalaVersions := List("2.10.4", "2.11.2")
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
    val state        = Keys.state.value
    val isPublishKey = key eq publish
    val suffix       = if (isPublishKey) "" else localSuffix

    def finish()                                  = subprojects foreach (p => state(crossScalaVersions in p) foreach (cross => publishProject(p, cross)))
    def publishProject(p: Project, cross: String) = state.set(version in p := state(version) + suffix, scalaVersion in p := cross) runTask (key in Compile in p)

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

trait TestOnly {
  self: Build.type =>

  def testDependencies = Def setting Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "org.scalacheck" %% "scalacheck" % "1.11.5" % "test"
  )

  lazy val testOnly = project.support dependsOn std settings (
                   name  :=  "psp-test",
            description  :=  "test encapsulation",
    testOptions in Test  +=  Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "1"),
    libraryDependencies <++= testDependencies,
      mainClass in Test  :=  Some("psp.tests.TestRunner"),
                   test  :=  (run in Test toTask "").value
  )
}

trait ConsoleOnly {
  self: Build.type =>

  def consoleCode = """
    import scala.collection.{ mutable, immutable }
    import psp.std._, ansi._
    implicit final class ReplForeachOps[A](val target: Foreach[A]) {
      def !> : Unit = println(target mkString EOL)
      def  >(implicit shows: Show[A]): Unit = println(target.joinLines)
    }
    implicit final class ReplOps[A](val target: A) {
      def !> : Unit = println(target)
      def >(implicit shows: Show[A]): Unit = println(target.to_s)
    }
    import spire.algebra._, spire.math._, spire.implicits._, com.google.common.collect._
  """

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
    initialCommands in console  :=  consoleCode
  )
}
