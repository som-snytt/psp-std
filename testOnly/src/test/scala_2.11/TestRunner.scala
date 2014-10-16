package psp
package tests

import org.scalacheck._
import org.scalacheck.Prop.forAll
import psp.std._, api._
import StdEq._

object TestRunner_211 extends TestRunnerCommon {
  def scalaVersion = "2.11"

  /** How to check for function equivalence? In the absence of mathematical breakthroughs,
   *  recursively throw scalacheck at it again, verifying arbitrary inputs have the same result.
   */
  implicit def Fun[A : Arbitrary : Eq] : Eq[Predicate[A]] =
    Eq[Predicate[A]]((f, g) => Test.check(forAll((x: A) => f(x) === g(x)))(identity).passed)

  override def bundles = Seq(
    new Typecheck,
    new AlgebraPoliceman[Boolean]("Boolean") { override def join = "||" ; override def meet = "&&" },
    new AlgebraPoliceman[Predicate[Int]]("Int => Boolean")(?, ?, Fun[Int])
    // new AlgebraPoliceman[inSet[Int]]("inSet[Int]") { override def join = "||" ; override def meet = "&&" }
  ) ++ super.bundles
}
