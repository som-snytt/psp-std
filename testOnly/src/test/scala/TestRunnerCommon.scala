package psp
package tests

import psp.std._
import Unsafe.universalEq

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

  def Try[A](expr: => A): Try[A] = try scala.util.Success(expr) catch {
    case t: sucControlThrowable => throw t
    case t: Throwable           => scala.util.Failure(t)
  }

  def wrapRun(b: Bundle): Boolean = Try(b.run) fold (t => andFalse(println(s"Caught $t running $b")), identity)

  def main(args: Array[String]): Unit = {
    bundles filter shouldRun mapOnto wrapRun filterValues (x => !x) match {
      case PolicyMap()        => println("\nAll tests passed.") ; if (isTestDebug) println(ansi.colorMap.to_s)
      case PolicyMap(ks @ _*) => println("Some tests failed in bundles: " + ks.mkString(", ")) ; throw new Exception
    }
  }
}
