package psp
package build

import sbt._, Keys._
import bintray.Plugin.bintraySettings
import com.typesafe.tools.mima.plugin.MimaKeys._
import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings
import psp.meta._

object Build extends sbt.Build with PublishOnly with ConsoleOnly with TestOnly {
  // scala.reflect.runtime.currentMirror.staticPackage("psp.core")
  def versionScalacOptions(binaryVersion: String): Seq[String] = {
    val xs1 = if (sys.props contains "debug") Seq("-Ylog:all") else Nil
    val xs2 = Seq("-Ywarn-dead-code", "-language:_")
    val xs3 = binaryVersion match {
      case "2.10" => Nil
      case "2.11" => Seq("-Ywarn-unused", "-Ywarn-unused-import")
    }
    xs1 ++ xs2 ++ xs3
  }
  def subprojects = List(api, std)

  implicit class ProjectOps(val p: Project) {
    def buildWith(vs: String*)        = p settings (crossScalaVersions := vs.toSeq)
    def sub(text: String): Project    = p also (bintraySettings ++ mimaDefaultSettings ++ common) settings (name := "psp-" + p.id, description := text)
    def support: Project              = p also common settings (publishArtifact := false)
    def also(ss: SettingSeq): Project = p settings (ss: _*)
    def common = Seq[Setting[_]](
                                  resolvers +=  "bintray/paulp" at "https://dl.bintray.com/paulp/maven",
                               scalaVersion :=  "2.11.2",
                         crossScalaVersions :=  Seq("2.10.4", "2.11.2"),
                                    version :=  ( if (hasReleaseProp) baseVersion else localVersion ),
                               organization :=  pspOrg,
                                logBuffered :=  false,
                                shellPrompt :=  (s => "%s#%s> ".format(name.value, s.currentProject)),
                              scalacOptions ++= versionScalacOptions(scalaBinaryVersion.value),
                               javacOptions ++= Seq("-nowarn", "-XDignore.symbol.file"),
                                   licenses :=  Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
                         logLevel in update :=  Level.Warn,
                    publishArtifact in Test :=  false,
                  parallelExecution in Test :=  false,
                               fork in Test :=  true,
                                 crossPaths :=  true,
                                    publish <<= publish dependsOn safePublish(p),
      unmanagedSourceDirectories in Compile +=  (sourceDirectory in Compile).value / s"scala${scalaBinaryVersion.value}",
         unmanagedSourceDirectories in Test +=  (sourceDirectory in Test).value / s"scala${scalaBinaryVersion.value}"
    )

    def root: Project = p in file(".") also common settings (
                 name :=  "psp-std-root",
          description :=  "psp's project which exists to please sbt",
      publishArtifact :=  false,
              publish :=  (),
             commands +=  Command.args("mima", "<version>")(mimaCommand),
              console <<= console in Compile in consoleOnly,
                 test <<= test in testOnly
    )
  }

  lazy val root = project.root aggregate (api, std)
  lazy val api  = project.sub("api for psp's non-standard standard library")
  lazy val std  = project.sub("psp's non-standard standard library") dependsOn api settings (publishArtifact := scalaBinaryVersion.value == "2.11")
}

trait PublishOnly {
  self: Build.type =>

  import scala.sys.process._

  lazy val baseVersion  = "0.3.1-M2"
  lazy val localVersion = baseVersion + localSuffix

  // It's a pretty hideous version string, but it should guarantee we never overwrite
  // artifacts and that dependency managers will see the local version numbers as increasing.
  def pspOrg           = "org.improving"
  def releaseProp      = "psp.release"
  def hasReleaseProp   = sys.props contains releaseProp
  def isRepoClean      = "git diff --quiet --exit-code HEAD".! == 0
  def repoSha: String  = "git rev-parse HEAD".!! take 10
  def repoDiff: String = ("git diff" #| "md5").!! take 10 // not using at present - hopefully "dirty" is enough
  def localSuffix      = "-%s-%s%s".format(dateTime, repoSha, if (isRepoClean) "" else "-dirty")

  def safePublish(p: Project): TaskOf[Unit] = Def task {
    if (!isRepoClean)
      try println(s"Can't publish with a dirty repository.") finally sys exit 1
    else if (!hasReleaseProp)
      try println(s"As a safeguard, publishing release artifacts requires -D$releaseProp.") finally sys exit 1
  }
}

trait TestOnly {
  self: Build.type =>

  /** mima command optionally takes a baseline version, e.g. sbt 'mima 0.1.0-M1'
   */
  def mimaCommand = stateCommand {
    case (s, Nil)            => mimaRun(s, stdArtifact("0.3.0-M8"))
    case (s, version :: Nil) => mimaRun(s, stdArtifact(version))
    case (s, _)              => s.fail
  }

  private def mimaRun(state: State, module: ModuleID): Unit =
    state.put(previousArtifact in std, Some(module)) runTask (reportBinaryIssues in std)

  // Mima won't resolve the %% cross version.
  def stdArtifact(version: String): ModuleID = pspOrg % "psp-std_2.11" % version

  def testDependencies = Def setting Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "org.scalacheck" %% "scalacheck" % "1.11.5" % "test"
  )

  lazy val testOnly = project.support dependsOn (api, std) settings (
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
    import spire.algebra._, spire.math._, com.google.common.collect._
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
    scalacOptions in console in Compile  :=  Seq("-language:_"),  // turning off all the noisy warnings
             initialCommands in console  :=  consoleCode
  )
}
