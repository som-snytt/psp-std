package psp
package tests

import psp.std._
import org.scalacheck._
import org.scalacheck.Prop.forAll

class AlgebraPoliceman[A : BooleanAlgebra : Arbitrary : Eq](val bundle: String) extends AlgebraLaws[A] with ScalacheckBundle {
  val algebra = ?[BooleanAlgebra[A]]
  import algebra._

  val props = Seq[NamedProp](
    "a ∧ (b ∧ c) = (a ∧ b) ∧ c"       -> forAll(associative(and)),
    "a ∨ (b ∨ c) = (a ∨ b) ∨ c"       -> forAll(associative(or)),
    "a ∧ b = b ∧ a"                   -> forAll(commutative(and)),
    "a ∨ b = b ∨ a"                   -> forAll(commutative(or)),
    "a ∧ (b ∨ c) = (a ∧ b) ∨ (a ∧ c)" -> forAll(distributive(and, or)),
    "a ∨ (b ∧ c) = (a ∨ b) ∧ (a ∨ c)" -> forAll(distributive(or, and)),
    "a ∧ (a ∨ b) = a"                 -> forAll(absorption(and, or)),
    "a ∨ (a ∧ b) = a"                 -> forAll(absorption(or, and)),
    "a ∨ 0 = a"                       -> forAll(identity(or, zero)),
    "a ∧ 1 = a"                       -> forAll(identity(and, one)),
    "a ∨ ¬a = 1"                      -> forAll(complement(or, one)),
    "a ∧ ¬a = 0"                      -> forAll(complement(and, zero))
  )
}
