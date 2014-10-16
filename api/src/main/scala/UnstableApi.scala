package psp
package std
package api

import scala._
import java.lang.String

/** Type classes I'm less certain about keeping.
 */

trait Generator[+A] extends Any { def apply(f: A => Unit): Generator[A] }
trait Zero[A] extends Any { def zero: A ; def isZero(x: A): Boolean }

/** The classic type class for encoding string representations.
 */
trait Show[-A] extends Any { def show(x: A): String }

trait Hash[-A] extends Any { def hash(x: A): Int }

/** A way of externalizing a String attachment on types.
 *  Used for example to attach a String to a function instance.
 *  There is a fallback no-op instance in the companion scope.
 *  TODO - should there be?
 */
trait Labelable[A] {
  def label(value: A, label: String): A
}
object Labelable {
  class LabelableIdentity[A] extends Labelable[A] {
    def label(value: A, label: String): A = value
  }
  implicit def labelIdentity[A] : Labelable[A] = new LabelableIdentity[A]
}

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

trait DerivedIntCompanion extends Any {
  type Family
  type Derived

  def intValue(x: Derived): Int
  def apply(n: Int): Derived
  def unapply(x: Family): Option[Int]

  def zero: Zero[Derived]
  def equiv: Eq[Derived]
  def ordering: Order[Derived]
  def shows: Show[Derived]
}
