package psp
package build

import sbt._
import Keys._
import Scope.ThisScope

object Sxr {
  val SxrConfig            = config("sxr") hide
  val sxr                  = TaskKey[File]("sxr")
  val sxrSourceDirectories = TaskKey[Seq[File]]("sxr-source-directories")

  def settings = inTask(sxr)(
    Seq(
       managedClasspath :=  update.value.matching(configurationFilter(SxrConfig.name)).classpath,
          scalacOptions +=  "-P:sxr:base-directory:" + sxrSourceDirectories.value.absString,
          scalacOptions +=  "-Xplugin:" + managedClasspath.value.files.filter(_.getName.contains("sxr")).absString,
          scalacOptions +=  "-Ymacro-no-expand",
          scalacOptions +=  "-Ystop-after:sxr",
                 target :=  target.in(taskGlobal).value / "browse",
      sxr in taskGlobal <<= sxrTask
    )
  )
  def taskGlobal = ThisScope.copy(task = Global)
  def sxrTask = (sources, target, scalacOptions, classpathOptions, scalaInstance, fullClasspath, streams) map { (srcs, out, opts, cpOpts, si, cp, s) =>
    val cache = s.cacheDirectory
    val outputDir = out.getParentFile / (out.getName + ".sxr")
    val f = FileFunction.cached(cache / "sxr", FilesInfo.hash) { in =>
      s.log.info("Generating sxr output in " + outputDir.getAbsolutePath + "...")
      IO.delete(out)
      IO.createDirectory(out)
      val comp = new compiler.RawCompiler(si, cpOpts, s.log)
      comp(in.toSeq.sorted, cp.files, out, opts)
      Set(outputDir)
    }
    f(srcs.toSet)
    outputDir
  }
}
