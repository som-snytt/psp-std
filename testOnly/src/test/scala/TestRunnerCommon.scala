package psp
package tests

import psp.std._

abstract class TestRunnerCommon {
  def scalaVersion: String
  def shouldRun(b: Bundle) = sys.props get "psp.bundles" match {
    case Some(s) => b.bundle contains s
    case _       => true
  }

  def bundles = Direct[Bundle](
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

  def wrapRun(b: Bundle): Boolean = Try(b.run) fold (t => andFalse(println(s"Caught $t running $b")), identity)

  def main(args: Array[String]): Unit = {
    bundles filter shouldRun mapOntoByEquals wrapRun filterValues (x => !x) match {
      case PolicyMap()        => println("\nAll tests passed.") ; if (isTestDebug) println(ansi.colorMap.to_s)
      case PolicyMap(ks @ _*) => println("Some tests failed in bundles: " + ks.mkString(", ")) ; throw new Exception
    }
  }
}
