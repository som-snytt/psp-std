package psp
package build

import scala.Predef.{ conforms => _ }
import sbt._, Keys._, psp.libsbt._, Deps._
import psp.std._

object Build extends sbt.Build {
  def stdArgs  = wordSeq("-Yno-predef -Yno-imports -Yno-adapted-args")
  def replArgs = wordSeq("-language:_ -Yno-predef -Yno-adapted-args")

  def javaBinaryVersion           = sys.props("java.specification.version")
  def javaCrossTarget(id: String) = key.buildBase mapValue (_ / "target" / id / s"java_$javaBinaryVersion")

  def rootResourceDir: SettingOf[File] = resourceDirectory in Compile in LocalRootProject
  def subprojects                      = List[sbt.Project](api, dmz, std, pio, dev)
  def classpathDeps                    = convertSeq(subprojects): List[ClasspathDep[ProjectReference]]
  def projectRefs                      = convertSeq(subprojects): List[ProjectReference]

  implicit class ProjectOps(val p: Project) {
    def setup(): Project             = p.alsoToolsJar also commonSettings(p) also (name := "psp-" + p.id)
    def setup(text: String): Project = setup() also (description := text)
    def usesCompiler                 = p settings (libraryDependencies += Deps.scalaCompiler.value)
    def usesReflect                  = p settings (libraryDependencies += Deps.scalaReflect.value)
    def helper(): Project            = p.noArtifacts setup "helper project" dependsOn (classpathDeps: _*)
    def noSources                    = p in file("target/helper/" + p.id)
  }

  private def commonSettings(p: Project) = standardSettings ++ Seq(
            resolvers +=  Resolver.mavenLocal,
              version :=  sbtBuildProps.buildVersion,
         scalaVersion :=  scalaVersionLatest,
   crossScalaVersions :=  Seq("2.10.4", scalaVersion.value),
             licenses :=  pspLicenses,
         organization :=  pspOrg,
        scalacOptions ++= scalacOptionsFor(scalaBinaryVersion.value) ++ stdArgs,
    publishMavenStyle :=  true
  ) ++ (
    if (p.id == "root") Nil
    else Seq(target <<= javaCrossTarget(p.id))
  )

  private def loudLog(msg: String)(f: String => Unit): Unit = {
    f("")
    f("<*> " + msg)
    f("")
  }

  def aggregateIn[A](key: TaskKey[A], project: Project) = {
    def label = key.key.label
    def nameOf(r: Reference): String = r match {
      case LocalProject(id) => id
      case _                => "" + r
    }
    val aggregated = (project: ProjectDefinition[ProjectReference]).aggregate map nameOf mkString ", "

    Command.command(label) { state =>
      loudLog(show"$label is aggregated in [ $aggregated ]")(s => state.log.info(s))
      state runAll (key in project)
    }
  }

  lazy val root = project.root.setup dependsOn (classpathDeps: _*) settings (
    commands ++= Seq(
      aggregateIn(clean, compileOnly),
      aggregateIn(compile in Compile, compileOnly),
      aggregateIn(test, testOnly),
      aggregateIn(key.packageTask, publishOnly),
      aggregateIn(publish, publishOnly),
      aggregateIn(publishLocal, publishOnly),
      aggregateIn(publishM2, publishOnly)
    ),
    console in Compile <<=  console in Compile in consoleOnly,
       console in Test <<=  console in Test in consoleOnly,
          watchSources <++= sources in Test in testOnly
  )

  lazy val api = project setup "api for psp's non-standard standard library"
  lazy val dmz = project setup "dmz for psp's non-standard standard library" dependsOn api
  lazy val std = project setup "psp's non-standard standard library" dependsOn dmz
  lazy val pio = project setup "io library for pps-std" dependsOn std
  lazy val dev = project setup "psp's even less stable code" dependsOn std

  lazy val publishOnly = project.helper.noSources aggregate (api, dmz, std)
  lazy val compileOnly = project.helper.noSources aggregate (projectRefs: _*)
  lazy val testOnly    = project.helper aggregate (projectRefs: _*) settings (
          testOptions in Test  +=  Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "1"),
    parallelExecution in Test  :=  false,
                  logBuffered  :=  false,
          libraryDependencies <++= testDependencies,
                         test  :=  (run in Test toTask "").value
  )

  // A console project which pulls in misc additional dependencies currently being explored.
  // Removing all scalac options except the ones listed here, to eliminate all the warnings
  // repl startup code in resources/initialCommands.scala
  lazy val consoleOnly = project.helper.usesCompiler.alsoToolsJar dependsOn (testOnly % "test->test") settings (
                    libraryDependencies <++= consoleDependencies,
    scalacOptions in (Compile, console)  :=  replArgs,
       scalacOptions in (Test, console)  :=  replArgs,
                           key.initRepl <+=  resourceDirectory in Compile mapValue (d => IO.read(d / "initialCommands.scala")),
                   key.initRepl in Test  +=  "\nimport org.scalacheck._, Prop._, Gen._\nimport psp.tests._"
  )

  def testDependencies = Def setting Seq(
    "org.scala-lang"  % "scala-reflect" % scalaVersion.value,
    "org.scalacheck" %% "scalacheck"    %      "1.11.5"       % "test"
  )
  def consoleDependencies = Def setting Seq(
    "org.scala-lang"            % "scala-compiler" % scalaVersion.value
    // "org.spire-math"           %% "spire"          %      "0.8.2",
    // "com.chuusai"              %% "shapeless"      %      "2.0.0",
    // "com.google.guava"          % "guava"          %       "17.0"
    // "com.google.code.findbugs"  % "jsr305"         %       "3.0.0"
  )
}
