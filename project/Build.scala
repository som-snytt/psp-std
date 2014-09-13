package psp
package build

import sbt._, Keys._, psp.libsbt._
import bintray.Plugin._

// with LibSbtStub
object Build extends sbt.Build with ConsoleOnly with LibSbt {
  // def subprojects = List(api, std)

  def is211 = Def setting (scalaBinaryVersion.value == "2.11")
  def versionedSource(config: Configuration) = Def setting (
    (sourceDirectory in config).value / "scala_%s".format(scalaBinaryVersion.value)
  )
  def versionedSourceSettings = List(Test, Compile) map (c => (unmanagedSourceDirectories in c) += versionedSource(c).value)

  private def commonSettings(p: Project) = versionedSourceSettings ++ Seq[Setting[_]](
                                    version :=  "0.4.0-M0", //publishVersion,
                               scalaVersion :=  scalaVersionLatest,
                         crossScalaVersions :=  scalaVersionsCross,
                                   licenses :=  pspLicenses,
                               organization :=  pspOrg,
                              // scalacOptions ++= scalacOptionsFor(scalaBinaryVersion.value),
                          publishMavenStyle :=  true,
                    javacOptions in Compile ++= Seq("-nowarn", "-XDignore.symbol.file"),
                   scalacOptions in Compile ++= Seq("-language:_"),
                         logLevel in update :=  Level.Warn,
                    publishArtifact in Test :=  false,
   publishArtifact in (Compile, packageDoc) :=  false,
   publishArtifact in (Compile, packageSrc) :=  false
  )

  implicit class ProjectOps(val p: Project) {
    def common: Project            = p settings (commonSettings(p): _*)
    def sub(text: String): Project = p.common settings (bintraySettings: _*) settings (name := "psp-" + p.id, description := text)
    def support: Project           = p.common settings (publishArtifact := false)
  }

  lazy val root = project.common in file(".") aggregate (api, std) settings (
           name :=  "psp-std-root",
    description :=  "psp's project which exists to please sbt",
        console <<= console in Compile in consoleOnly,
           test <<= test in testOnly
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
}

// trait PublishOnly {
//   self: Build.type =>

//   import scala.sys.process._

//   def isRepoClean    = Process("git diff --quiet --exit-code HEAD").! == 0
//   def hasReleaseProp = sys.props contains pspReleaseProp

//   def safePublish(p: Project): TaskOf[Unit] = Def task {
//     if (!isRepoClean)
//       try println(s"Can't publish with a dirty repository.") finally sys exit 1
//     else if (!hasReleaseProp)
//       try println(s"As a safeguard, publishing release artifacts requires -D$pspReleaseProp.") finally sys exit 1
//   }
// }
// commands +=  Command.args("mima", "<version>")(mimaCommand),

// trait TestOnly {
//   self: Build.type =>

//   // /** mima command optionally takes a baseline version, e.g. sbt 'mima 0.1.0-M1'
//   //  */
//   // def mimaCommand = stateCommand {
//   //   case (s, Nil)            => mimaRun(s, stdArtifact("0.3.1-M1"))
//   //   case (s, version :: Nil) => mimaRun(s, stdArtifact(version))
//   //   case (s, _)              => s.fail
//   // }

//   // private def mimaRun(state: State, module: ModuleID): Unit =
//   //   state.set(previousArtifact in std := Some(module)) runTask (MimaKeys.reportBinaryIssues in std)

//   // def stateCommand(f: (State, List[String]) => Unit): (State, Seq[String]) => State =
//   //   (state, args) => try state finally f(state, args.toList)

//   // Mima won't resolve the %% cross version.
//   // def stdArtifact(version: String): ModuleID = pspOrg % "psp-std_2.11" % version

//   lazy val testOnly = project.support dependsOn (api, std) settings (
//                    name :=  "psp-test",
//             description :=  "test encapsulation",
//     testOptions in Test +=  Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "1"),
//     libraryDependencies ++= Seq(scalaReflect(scalaVersion.value), scalacheck),
//       mainClass in Test :=  Some("psp.tests.TestRunner"),
//                    test :=  (run in Test toTask "").value
//   )
// }

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
