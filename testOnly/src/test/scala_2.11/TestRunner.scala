package psp
package tests

import psp.std._
import org.scalacheck._
import org.scalacheck.Prop.forAll

object TestRunner_211 extends TestRunnerCommon {
  def scalaVersion = "2.11"

  // Set equivalence can't use scala's equals, which will not terminate
  // on set complement.
  import ScalaSet.scalaSetEq

  /** How to check for function equivalence? In the absence of mathematical breakthroughs,
   *  recursively throw scalacheck at it again, verifying arbitrary inputs have the same result.
   */
  implicit def Fun[A : Arbitrary : Eq] : Eq[Predicate[A]] =
    Eq[Predicate[A]]((f, g) => Test.check(forAll((x: A) => f(x) === g(x)))(identity).passed)

  override def bundles = super.bundles ++ Seq(
    new Typecheck,
    // These cause some kind of "erroneous type" error in scala 2.10 and fuck if I can figure out why.
    new AlgebraPoliceman[Boolean]("Boolean") { override def join = "||" ; override def meet = "&&" },
    new AlgebraPoliceman[Predicate[Int]]("Int => Boolean")(?, ?, Fun[Int]),
    new AlgebraPoliceman[Set[Int]]("Set[Int]") { override def join = "||" ; override def meet = "&&" }
  )
}
