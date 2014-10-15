package psp
package std
package api

import scala.Any
import scala.Boolean
import scala.Unit
import scala.Int
import scala.Long
import java.lang.String

/** The classic type class for encoding value equivalence.
 */
trait Eq[-A] extends Any { def equiv(x: A, y: A): Boolean }

/** The classic type class for turning string representations into typed values.
 */
trait Read[A] extends Any { def read(s: String): A }

/** The classic type class for encoding string representations.
 */
trait Show[-A] extends Any { def show(x: A): String }

/** Contravariance vs. implicits, the endless battle.
 *  We return a java three-value enum from compare in preference
 *  to a wild stab into the 2^32 states of an Int. This is a
 *  controversial thing to do, in the year 2014. Not joking.
 */
trait Order[-A]          extends Any                                 { def compare(x: A, y: A): Cmp             }
trait PartialOrder[-A]   extends Any                                 { def partialCompare(lhs: A, rhs: A): PCmp }
trait OrderEq[-A]        extends Any with Order[A] with Eq[A]
trait PartialOrderEq[-A] extends Any with PartialOrder[A] with Eq[A]

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

trait IndexOrNth extends Any {
  type This <: IndexOrNth

  def +(n: Int): This
  def -(n: Int): This
  def next: This
  def prev: This

  def isUndefined: Boolean
  def value: Int
  def toInt: Int
  def toLong: Long
  def toNth: Nth
  def toIndex: Index

  // Concessions to reality
  def intIndex: Int
  def intNth: Int
}

/** The builder type class.
 */
trait Builds[-Elem, +To] extends Any { def build(xs: Foreach[Elem]): To }

/** Collections classes.
 */
trait Index extends Any with IndexOrNth        { type This = Index ; def value: Int }
trait Nth extends Any with IndexOrNth          { type This = Nth ; def value: Int }
trait Size extends Any                         { def value: Int                  }
trait Invariant[A] extends Any                 { def contains(x: A): Boolean     }
trait Foreach[+A] extends Any with HasSizeInfo { def foreach(f: A => Unit): Unit }
trait Indexed[+A] extends Any with Foreach[A]  { def elemAt(index: Index): A     }
trait Direct[+A] extends Any with Indexed[A]   { def size: Size                  }

trait Linear[+A] extends Any with Foreach[A] {
  type Tail <: Linear[A]
  def isEmpty: Boolean
  def head: A
  def tail: Tail
}
