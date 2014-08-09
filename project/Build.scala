package psp
package build

import sbt._, Keys._

object Build extends sbt.Build {
  def imports = """
    import java.{ lang => jl, util => ju }
    import java.nio.{ file => jnf }
    import psp.std._
  """.trim

  def common = Seq[Setting[_]](
       organization :=  "org.improving",
            version :=  "0.1.0-SNAPSHOT",
       scalaVersion :=  "2.11.2",
        logBuffered :=  false,
      scalacOptions ++= Seq("-language:_"),
       javacOptions ++= Seq("-nowarn", "-XDignore.symbol.file")
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
