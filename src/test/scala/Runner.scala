package psp
package stdtests

import psp.std._
import org.scalacheck._
import org.scalacheck.Prop.forAll

object TestRunner {
  // Set equivalence can't use scala's equals, which will not terminate
  // on set complement.
  import ScalaSet.scalaSetEq

  /** How to check for function equivalence? In the absence of mathematical breakthroughs,
   *  recursively throw scalacheck at it again, verifying arbitrary inputs have the same result.
   */
  implicit def Fun[A : Arbitrary : Eq] = Eq[Predicate[A]]((f, g) => Test.check(forAll((x: A) => f(x) === g(x)))(identity).passed)

  def main(args: Array[String]): Unit = {
    AlgebraPoliceman.check[Boolean]("Boolean")
    AlgebraPoliceman.check[Predicate[Int]]("Predicate[Int]")
    AlgebraPoliceman.check[Set[Int]]("Set[Int]")

    val tests = new StdTests
    val msg = tests.run
    if (tests.failed)
      throw new Exception(msg)
    else
      println(msg)
  }
}
