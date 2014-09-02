package psp
package tests

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
  implicit def Fun[A : Arbitrary : Eq] : Eq[Predicate[A]] =
    Eq[Predicate[A]]((f, g) => Test.check(forAll((x: A) => f(x) === g(x)))(identity).passed)

  def main(args: Array[String]): Unit = {
    val bundles = Seq[Bundle](
      new AlgebraPoliceman[Boolean]("Boolean") { override def join = "||" ; override def meet = "&&" },
      new AlgebraPoliceman[Predicate[Int]]("Int => Boolean")(?, ?, Fun[Int]),
      new AlgebraPoliceman[Set[Int]]("Set[Int]") { override def join = "||" ; override def meet = "&&" },
      new ValuesSpec,
      new SizeInfoSpec,
      new OperationCounts,
      new Nats,
      new Collections,
      new Typecheck
    )
    val results = bundles mapOnto (_.run)
    results.keys.toList filterNot results match {
      case Nil => println("\nAll tests passed.")
      case ks  => println("Some tests failed in bundles: " + ks.mkString(", ")) ; throw new Exception
    }
  }
}
