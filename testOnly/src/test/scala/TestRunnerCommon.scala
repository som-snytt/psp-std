package psp
package tests

import psp.std._
import Unsafe.universalEq

abstract class TestRunnerCommon {
  def scalaVersion: String

  def bundles: Seq[Bundle] = Seq(
    new StringExtensions,
    new ValuesSpec,
    new SizeInfoSpec,
    new InferenceSpec,
    new Collections,
    new SliceSpec,
    new OperationCounts(scalaVersion)
  )
  def main(args: Array[String]): Unit = {
    val results = bundles mapOnto (_.run)
    results filterValues (x => !x) match {
      case PolicyMap()        => println("\nAll tests passed.")
      case PolicyMap(ks @ _*) => println("Some tests failed in bundles: " + ks.mkString(", ")) ; throw new Exception
    }
  }
}
