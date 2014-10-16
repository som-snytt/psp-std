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
trait TryShow[-A] extends Any { def show(x: A): String }

trait Hash[-A] extends Any { def hash(x: A): Int }
trait TryHash[-A] extends Any { def hash(x: A): Int }

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

/** Offering a last-resort fallback implicit absolutely requires a companion object
 *  for that type, because the companion scope will never be searched at all if the
 *  fallback is placed in the normal scope. An example of getting this wrong is the
 *  presence of fallbackStringCanBuildFrom in Predef, which despite being "low priority"
 *  is always favored over any companion-defined implicit.
 */
trait LowTryShow {
  implicit def hasNoShow[A] : TryShow[A] = TryShow.NoShow
}
object TryShow extends LowTryShow {
  final class HasShow[-A](shows: Show[A]) extends TryShow[A] { def show(x: A) = shows show x }
  final object NoShow extends TryShow[Any] { def show(x: Any): String = "" + x }

  implicit def hasShow[A](implicit shows: Show[A]): HasShow[A] = new HasShow(shows)
}

trait LowTryHash {
  implicit def hasNoHash[A] : TryHash[A] = TryHash.NoHash
}
object TryHash extends LowTryHash {
  final class HasHash[-A](hashes: Hash[A]) extends TryHash[A] { def hash(x: A): Int = hashes hash x }
  final object NoHash extends TryHash[Any] { def hash(x: Any): Int = x.## }

  implicit def hasHash[A](implicit z: Hash[A]): HasHash[A] = new HasHash(z)
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
