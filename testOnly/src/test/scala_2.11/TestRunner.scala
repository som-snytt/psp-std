package psp
package tests

import psp.std._, api._
import org.scalacheck._, Prop.forAll
import StdEq.booleanEq

object TestRunner_211 extends TestRunnerCommon {
  def scalaVersion = "2.11"

  /** Scala, you suck.
   *  [error] could not find implicit value for parameter equiv: psp.std.api.Eq[psp.tests.Pint => psp.std.Boolean]
   *  The parameter can be given explicitly, it just won't be found unless the function type is invariant.
   */
  type InvariantFunction[T, +R] = T => R
  type InvariantPredicate[A]    = A => Boolean

  /** How to check for function equivalence? In the absence of mathematical breakthroughs,
   *  recursively throw scalacheck at it again, verifying arbitrary inputs have the same result.
   */
  def observationalEq[M[X], A : Arbitrary, B : Eq](f: (M[A], A) => B): Eq[M[A]] = Eq[M[A]] { (xs, ys) =>
    val prop = forAll((elem: A) => f(xs, elem) === f(ys, elem))
    (Test check prop)(identity).passed
  }

  implicit def pintHashEq: HashEq[Pint]                                      = HashEq.natural[Pint]()
  implicit def pintShow: Show[Pint]                                          = Show.natural[Pint]()
  implicit def predicateEq[A : Arbitrary] : Eq[InvariantPredicate[A]]        = observationalEq[InvariantPredicate, A, Boolean](_ apply _)
  implicit def intensionalEq[A : Arbitrary : HashEq] : Eq[IntensionalSet[A]] = observationalEq[IntensionalSet, A, Boolean](_ apply _)

  override def bundles = Direct[Bundle](
    new Typecheck,
    new Collections_211,
    new AlgebraPoliceman[Boolean]("Boolean") { override def join = "||" ; override def meet = "&&" },
    new AlgebraPoliceman[InvariantPredicate[Pint]]("InvariantPredicate[Pint]"),
    new AlgebraPoliceman[IntensionalSet[Pint]]("IntensionalSet[Pint]")
  ) ++ super.bundles
}
