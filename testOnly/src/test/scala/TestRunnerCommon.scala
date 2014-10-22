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
    new PolicyBasic,
    new ValuesSpec,
    new SizeSpec,
    new InferenceSpec,
    new Collections,
    new SliceSpec,
    new OperationCounts(scalaVersion)
  )

  def Try[A](expr: => A): Try[A] = try scala.util.Success(expr) catch {
    case t: ControlThrowable => throw t
    case t: Throwable        => scala.util.Failure(t)
  }

  def wrapRun(b: Bundle): Boolean = Try(b.run).fold(x => x, t => andFalse(println(s"Caught $t running $b")))

  def main(args: Array[String]): Unit = {
    bundles filter shouldRun mapOnto wrapRun filterValues (x => !x) match {
      case PolicyMap()        => println("\nAll tests passed.")
      case PolicyMap(ks @ _*) => println("Some tests failed in bundles: " + ks.mkString(", ")) ; throw new Exception
    }
  }
}
