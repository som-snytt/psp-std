package psp
package build

import sbt._, Keys._, psp.libsbt._

object Build extends sbt.Build with PublishOnly with ConsoleOnly with TestOnly {
  // scala.reflect.runtime.currentMirror.staticPackage("psp.core")
  def versionedScalacOptions = Def setting {
    val xs1 = if (sys.props contains "debug") Seq("-Ylog:all") else Nil
    val xs2 = if (is211.value) Seq("-Ywarn-unused", "-Ywarn-unused-import") else Nil

    Seq("-Ywarn-dead-code", "-language:_") ++ xs1 ++ xs2
  }

  def isDebug             = sys.props contains "debug"
  def is211               = Def setting (scalaBinaryVersion.value == "2.11")
  def versionedSourceName = Def setting ("scala" + scalaBinaryVersion.value)

  // XXX temp
  def incrementVersion(v: String): String = {
    val num = (v.reverse takeWhile (_.isDigit)).reverse
    (v dropRight num.length) + (num.toInt + 1).toString
  }


  def subprojects = List(api, std)
  private def localSuffix = "-" + dateTime
  private lazy val stableVersion = sys.props get "release.version" match {
    case Some(v) => v
    case _       => incrementVersion(pspApiRelease) + ( if (hasReleaseProp) "" else localSuffix )
  }
  private def commonSettings(p: Project) = Seq[Setting[_]](
                             scalaVersion :=  scalaVersionLatest,
                       crossScalaVersions :=  scalaVersionsCross,
                                  version :=  stableVersion,
                             organization :=  pspOrg,
                              logBuffered :=  false,
                              shellPrompt :=  (s => "%s#%s> ".format(name.value, s.currentRef.project)),
                            scalacOptions ++= versionedScalacOptions.value,
                             javacOptions ++= Seq("-nowarn", "-XDignore.symbol.file"),
                                 licenses :=  pspLicenses,
                       logLevel in update :=  Level.Warn,
                  publishArtifact in Test :=  false,
                parallelExecution in Test :=  false,
                             fork in Test :=  true,
                               crossPaths :=  true,
                                  publish <<= publish dependsOn safePublish(p),
    unmanagedSourceDirectories in Compile +=  (sourceDirectory in Compile).value / versionedSourceName.value,
       unmanagedSourceDirectories in Test +=  (sourceDirectory in Test).value / versionedSourceName.value
  )

  implicit class ProjectOps(val p: Project) {
    def common: Project                  = p also commonSettings(p)
    def buildWith(vs: String*)           = p settings (crossScalaVersions := vs.toSeq)
    def sub(text: String): Project       = p.common also (bintraySettings ++ mimaDefaultSettings) settings (name := "psp-" + p.id, description := text)
    def support: Project                 = p.common settings (publishArtifact := false)
  }

  lazy val root = project.root.common aggregate (api, std) settings (
           name :=  "psp-std-root",
    description :=  "psp's project which exists to please sbt",
       commands +=  Command.args("mima", "<version>")(mimaCommand),
        console <<= console in Compile in consoleOnly,
           test <<= test in testOnly
  )
  lazy val api  = project.sub("api for psp's non-standard standard library")
  lazy val std  = project.sub("psp's non-standard standard library") dependsOn api settings (publishArtifact := is211.value)
}

trait PublishOnly {
  self: Build.type =>

  import scala.sys.process._

  def isRepoClean    = Process("git diff --quiet --exit-code HEAD").! == 0
  def hasReleaseProp = sys.props contains pspReleaseProp

  def safePublish(p: Project): TaskOf[Unit] = Def task {
    if (!isRepoClean)
      try println(s"Can't publish with a dirty repository.") finally sys exit 1
    else if (!hasReleaseProp)
      try println(s"As a safeguard, publishing release artifacts requires -D$pspReleaseProp.") finally sys exit 1
  }
}

trait TestOnly {
  self: Build.type =>

  /** mima command optionally takes a baseline version, e.g. sbt 'mima 0.1.0-M1'
   */
  def mimaCommand = stateCommand {
    case (s, Nil)            => mimaRun(s, stdArtifact("0.3.1-M1"))
    case (s, version :: Nil) => mimaRun(s, stdArtifact(version))
    case (s, _)              => s.fail
  }

  def stateCommand(f: (State, List[String]) => Unit): (State, Seq[String]) => State =
    (state, args) => state doto (s => f(s, args.toList))

  private def mimaRun(state: State, module: ModuleID): Unit =
    state.set(previousArtifact in std := Some(module)) runTask (MimaKeys.reportBinaryIssues in std)

  // Mima won't resolve the %% cross version.
  def stdArtifact(version: String): ModuleID = pspOrg % "psp-std_2.11" % version

  lazy val testOnly = project.support dependsOn (api, std) settings (
                   name :=  "psp-test",
            description :=  "test encapsulation",
    testOptions in Test +=  Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "1"),
    libraryDependencies ++= Seq(Deps.scalaReflect.value, Deps.scalacheck),
      mainClass in Test :=  Some("psp.tests.TestRunner"),
                   test :=  (run in Test toTask "").value
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
