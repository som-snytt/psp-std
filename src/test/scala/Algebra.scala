package psp
package tests

import psp.std._
import org.scalacheck._
import org.scalacheck.Prop.forAll

class AlgebraPoliceman[A : BooleanAlgebra : Arbitrary : Eq](val bundle: String) extends AlgebraLaws[A] with ScalacheckBundle {
  val algebra = ?[BooleanAlgebra[A]]
  import algebra._

  def join = "∨"
  def meet = "^"

  def props = Seq[NamedProp](
    s"$bundle   a $meet (b $meet c) = (a $meet b) $meet c"           -> forAll(associative(and)),
    s"$bundle   a $join (b $join c) = (a $join b) $join c"           -> forAll(associative(or)),
    s"$bundle   a $meet b = b $meet a"                               -> forAll(commutative(and)),
    s"$bundle   a $join b = b $join a"                               -> forAll(commutative(or)),
    s"$bundle   a $meet (b $join c) = (a $meet b) $join (a $meet c)" -> forAll(distributive(and, or)),
    s"$bundle   a $join (b $meet c) = (a $join b) $meet (a $join c)" -> forAll(distributive(or, and)),
    s"$bundle   a $meet (a $join b) = a"                             -> forAll(absorption(and, or)),
    s"$bundle   a $join (a $meet b) = a"                             -> forAll(absorption(or, and)),
    s"$bundle   a $join 0 = a"                                       -> forAll(identity(or, zero)),
    s"$bundle   a $meet 1 = a"                                       -> forAll(identity(and, one)),
    s"$bundle   a $join ¬a = 1"                                      -> forAll(complement(or, one)),
    s"$bundle   a $meet ¬a = 0"                                      -> forAll(complement(and, zero))
  )
}
