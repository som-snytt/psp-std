package psp
package tests

import psp.std._

abstract class TestRunnerCommon {
  def scalaVersion: String

  def bundles: Seq[Bundle] = Seq(
    new ValuesSpec,
    new SizeInfoSpec,
    new OperationCounts(scalaVersion),
    new Nats,
    new Collections
  )
  def main(args: Array[String]): Unit = {
    val results = bundles mapOnto (_.run)
    results.keys.toList filterNot results match {
      case Nil => println("\nAll tests passed.")
      case ks  => println("Some tests failed in bundles: " + ks.mkString(", ")) ; throw new Exception
    }
  }
}
