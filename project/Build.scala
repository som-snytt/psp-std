package psp
package build

import sbt._, Keys._

object Build extends sbt.Build {
  def imports = """
    import java.{ lang => jl, util => ju }
    import java.nio.{ file => jnf }
    import psp.std._
  """.trim

  def cleanTest = Def task {
    (clean in Compile in Test).value
    (run in Test).toTask("").value
    ()
  }

  def common = Seq[Setting[_]](
       organization :=  "org.improving",
            version :=  "0.0.1-SNAPSHOT",
       scalaVersion :=  "2.11.2",
        logBuffered :=  false,
      scalacOptions ++= Seq("-language:_"),
       javacOptions ++= Seq("-nowarn", "-XDignore.symbol.file")
  )

  /** What is accomplished by this structure?
   *   - std does not depend on scala-reflect
   *   - macros can use the methods defined in std
   *   - when testing std, we have access to macros
   *   - after touching a testfile the macro runs and the test runs, but the entire project isn't rebuilt
   *  It's harder to get all these at once than you think.
   */
  lazy val root = project in file(".") dependsOn (macros, std) aggregate (macros, std) settings (common: _*) settings (
                          name :=  "psp-std-root",
                   shellPrompt :=  (s => "%s#%s> ".format(name.value, (Project extract s).currentRef.project)),
    initialCommands in console :=  imports,
                       publish :=  (),
                  publishLocal :=  (),
                          test <<= run in Test toTask "" dependsOn (clean in Test)
  )

  lazy val std = project settings (common: _*) settings (name := "psp-std")

  lazy val macros = project dependsOn std settings (common: _*) settings (
                   name := "psp-std-macros",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )
}
