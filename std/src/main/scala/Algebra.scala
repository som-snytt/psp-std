package psp
package std

/** A boolean algebra abstracts across the standard boolean operations.
 *  The given laws must hold.
 *
 *    a ∨ (b ∨ c) = (a ∨ b) ∨ c, a ∧ (b ∧ c) = (a ∧ b) ∧ c                associativity
 *    a ∨ b = b ∨ a, a ∧ b = b ∧ a                                        commutativity
 *    a ∨ (a ∧ b) = a, a ∧ (a ∨ b) = a                                    absorption
 *    a ∨ 0 = a, a ∧ 1 = a                                                identity
 *    a ∨ (b ∧ c) = (a ∨ b) ∧ (a ∨ c), a ∧ (b ∨ c) = (a ∧ b) ∨ (a ∧ c)    distributivity
 *    a ∨ ¬a = 1, a ∧ ¬a = 0                                              complements
 */
trait BooleanAlgebra[R] {
  def and(x: R, y: R): R
  def or(x: R, y: R): R
  def not(x: R): R
  def zero: R
  def one: R
}

object BooleanAlgebra {
  final case object ConstantTrue extends Predicate[Any] { def apply(x: Any): Boolean = true }
  final case object ConstantFalse extends Predicate[Any] { def apply(x: Any): Boolean = false }

  final case class MappedAlgebra[R, S](algebra: BooleanAlgebra[R], f: S => R, g: R => S) extends BooleanAlgebra[S] {
    def and(x: S, y: S): S = g(algebra.and(f(x), f(y)))
    def or(x: S, y: S): S  = g(algebra.or(f(x), f(y)))
    def not(x: S): S       = g(algebra.not(f(x)))
    def zero: S            = g(algebra.zero)
    def one: S             = g(algebra.one)
  }

  implicit object IdentityAlgebra extends BooleanAlgebra[Boolean] {
    def and(x: Boolean, y: Boolean): Boolean = x && y
    def or(x: Boolean, y: Boolean): Boolean  = x || y
    def not(x: Boolean): Boolean             = !x
    def zero: Boolean                        = false
    def one: Boolean                         = true
  }
  class PredicateAlgebra[A] extends BooleanAlgebra[Predicate[A]] {
    private type R = Predicate[A]

    def and(x: R, y: R): R = p => x(p) && y(p)
    def or(x: R, y: R): R  = p => x(p) || y(p)
    def not(x: R): R       = p => !x(p)
    def zero: R            = ConstantFalse
    def one: R             = ConstantTrue
  }

  implicit def predicateAlgebra[A] : PredicateAlgebra[A] = new PredicateAlgebra[A]
  implicit def scalaSetAlgebra[A] : ScalaSet.Algebra[A]  = new ScalaSet.Algebra[A]
}
