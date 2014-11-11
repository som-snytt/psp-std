package psp
package tests

import psp.std._, api._

abstract class TestRunnerCommon {
  def scalaVersion: String
  def bundles: Direct[Bundle]

  def shouldRun(b: Bundle) = sys.props get "psp.bundles" match {
    case Some(s) => b.bundle contains s
    case _       => true
  }

  def commonBundles = Direct[Bundle](
    new StringExtensions,
    new GridSpec,
    new PolicyBasic,
    new ValuesSpec,
    new SizeSpec,
    new InferenceSpec,
    new CollectionsSpec,
    new SliceSpec,
    new OperationCounts
  )

  def wrapRun(b: Bundle): Boolean = Try(b.run) fold (
    t => andFalse(println(s"Caught $t running $b"), t.printStackTrace),
    identity
  )

  def main(args: Array[String]): Unit = {
    (bundles filter shouldRun).byEquals mapOnto wrapRun filterValues (x => !x) match {
      case ExMap()        => println("\nAll tests passed.") ; if (isTestDebug) println(ansi.colorMap.to_s)
      case ExMap(ks @ _*) => println("Some tests failed in bundles: " + ks.mkString(", ")) ; throw new Exception
    }
  }
}
