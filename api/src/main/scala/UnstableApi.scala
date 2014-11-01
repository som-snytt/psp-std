package psp
package std
package api

import scala._
import java.lang.String

/** Type classes I'm less certain about keeping.
 */

trait Zero[A] extends Any       { def zero: A ; def isZero(x: A): Boolean }
trait Empty[+A] extends Any     { def empty: A                            }
trait Hash[-A] extends Any      { def hash(x: A): Int                     }
trait Sums[A] extends Any       { def zero: A ; def sum(x: A, y: A): A    }
trait Products[A] extends Any   { def one: A ; def product(x: A, y: A): A }

trait HashEq[-A] extends Any with Hash[A] with Eq[A]

/** The classic type class for encoding string representations.
 */
trait Show[-A] extends Any { def show(x: A): String }

trait Recover[A] extends Any { def onFailure[A](t: Throwable): A }

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
