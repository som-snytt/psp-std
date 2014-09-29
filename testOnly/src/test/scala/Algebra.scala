package psp
package tests

import org.scalacheck._
import org.scalacheck.Prop.forAll
import psp.std._, api._

class AlgebraPoliceman[A](val bundle: String)(implicit algebra: BooleanAlgebra[A], arb: Arbitrary[A], equiv: Eq[A]) extends AlgebraLaws[A] with ScalacheckBundle {
  import algebra._

  def join = "∨"
  def meet = "∧"
  def not  = "¬"

  def props = Seq[NamedProp](
    s"--- Boolean Algebra laws for type $bundle"             -> Prop.passed,
    s"  a $meet (b $meet c) = (a $meet b) $meet c"           -> forAll(associative(and)),
    s"  a $join (b $join c) = (a $join b) $join c"           -> forAll(associative(or)),
    s"  a $meet b = b $meet a"                               -> forAll(commutative(and)),
    s"  a $join b = b $join a"                               -> forAll(commutative(or)),
    s"  a $meet (b $join c) = (a $meet b) $join (a $meet c)" -> forAll(distributive(and, or)),
    s"  a $join (b $meet c) = (a $join b) $meet (a $join c)" -> forAll(distributive(or, and)),
    s"  a $meet (a $join b) = a"                             -> forAll(absorption(and, or)),
    s"  a $join (a $meet b) = a"                             -> forAll(absorption(or, and)),
    s"  a $join 0 = a"                                       -> forAll(identity(or, zero)),
    s"  a $meet 1 = a"                                       -> forAll(identity(and, one)),
    s"  a $join ${not}a = 1"                                 -> forAll(complement(or, one)),
    s"  a $meet ${not}a = 0"                                 -> forAll(complement(and, zero)),
    "---"                                                    -> Prop.passed
  )
}
