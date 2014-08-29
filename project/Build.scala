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

  def ifBinary[A](version: String)(thenp: => A, elsep: => A): TaskOf[A] =
    scalaBinaryVersion map { case `version` => thenp ; case _ => elsep }

  def imports = """
    import scala.collection.{ mutable, immutable }
    import psp.std._, api._, ansi._
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
      libraryDependencies +=  "org.improving" %% "sxr" % "1.0.1" % SxrConfig.name,
           sources in sxr <<= deepTasks(sources in Compile),
     sxrSourceDirectories <<= deep(sourceDirectories in Compile).map(_.flatten), // to properly relativize the source paths
     fullClasspath in sxr <<= deepTasks(externalDependencyClasspath in Compile)
  )
  def common = bintraySettings ++ mimaDefaultSettings ++ Seq[Setting[_]](
                              resolvers  +=  "bintray/paulp" at "https://dl.bintray.com/paulp/maven",
                           organization  :=  pspOrg,
                           scalaVersion  :=  "2.11.2",
                                version  :=  "0.3.0-M3",
                            logBuffered  :=  false,
                          scalacOptions ++=  opts ++ Seq("-Ywarn-dead-code", "-language:_"),
                          scalacOptions <++= ifBinary("2.11")("-Ywarn-unused" :: "-Ywarn-unused-import" :: Nil, Nil),
                           javacOptions ++=  Seq("-nowarn", "-XDignore.symbol.file"),
                               licenses  :=  Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
                publishArtifact in Test  :=  false,
              parallelExecution in Test  :=  false,
                           fork in Test  :=  true,
    scalacOptions in console in Compile  :=  Seq("-language:_")
  )

  def subprojects = List(api, std, macros)

  /** mima command optionally takes a baseline version, e.g. sbt 'mima 0.1.0-M1'
   */
  def mimaCommand = stateCommand {
    case (s, Nil)            => mimaRun(s, stdArtifact("0.2.0-M2"))
    case (s, version :: Nil) => mimaRun(s, stdArtifact(version))
    case (s, _)              => s.fail
  }

  private def mimaRun(state: State, module: ModuleID): Unit =
    state.put(previousArtifact in std, Some(module)) runTask (reportBinaryIssues in std)

  def runTestTask = run in Test toTask "" dependsOn (Keys.`package` in Compile) dependsOn (clean in Test)

  /** What is accomplished by this structure?
   *
   *   - std does not depend on scala-reflect
   *   - the macro project can make use of psp.std
   *   - when testing psp.std, we have access to psp.macros
   *   - after touching a testfile, the macro runs and the tests run, but the entire project isn't rebuilt
   *
   *  It's harder to get all those at once than you may think.
   */
  lazy val root = project in file(".") dependsOn (api, std, macros) aggregate (api, std, macros) also common also sourceDirSettings settings (
                                   name :=  "psp-std-root",
                            description :=  "psp's project which exists to please sbt",
                     crossScalaVersions :=  List("2.10.4", "2.11.2"),
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
                                   test :=  runTestTask.value
  )

  implicit class ProjectOps(val p: Project) {
    def sxr: Project                  = p configs SxrConfig settings (fullSxrSettings: _*)
    def also(ss: SettingSeq): Project = p settings (ss: _*)
  }

  // A project with misc dependencies currently being explored.
  lazy val depends = project also common settings (
                          name := "psp-deps",
                   description := "code with bonus dependecies",
           libraryDependencies ++= Seq(
              "org.spire-math"           %% "spire"     % "0.8.2",
              "com.chuusai"              %% "shapeless" % "2.0.0",
              "com.google.guava"          % "guava"     % "17.0",
              "net.sourceforge.findbugs"  % "jsr305"    % "1.3.7"
          ),
    initialCommands in console ~= (_ + "\nimport spire.algebra._, spire.math._, spire.implicits._, com.google.common.collect._")
  )

  def sourceDirsFor(version: String)(ifp: Seq[File], thenp: Seq[File]): Seq[File] = if (version == "2.11") ifp else thenp
  def sourceDirSettings = Seq(
    unmanagedSourceDirectories in Compile := sourceDirsFor(scalaBinaryVersion.value)((unmanagedSourceDirectories in Compile).value, Nil),
       unmanagedSourceDirectories in Test := sourceDirsFor(scalaBinaryVersion.value)((unmanagedSourceDirectories in Test).value, Seq(baseDirectory.value / "src/test/scala210"))
  )

  lazy val std = project.sxr dependsOn api also common also sourceDirSettings settings (
                    name := "psp-std",
             description := "psp's non-standard standard library",
      crossScalaVersions := List("2.11.2")
  )

  lazy val macros = project dependsOn (api, std) also common also sourceDirSettings settings (
                   name := "psp-std-macros",
            description := "macros for psp's non-standard standard library",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
     crossScalaVersions := List("2.11.2")
  )

  lazy val api = project.sxr also common settings (
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
    val state = Keys.state.value
    def isPublishKey  = key eq publish
    def versionString = state(version) + ( if (isPublishKey) "" else localSuffix )
    def finish()      = subprojects foreach (p => state(crossScalaVersions in p) foreach (cross => publishProject(p, cross)))

    def publishProject(p: Project, cross: String) = state.set(version in p := versionString, scalaVersion in p := cross) runTask (key in Compile in p)

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
