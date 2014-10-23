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
trait Read[A] extends Any { def read(x: String): A }

/** The collections builder type class. Not especially classic in this presentation.
 */
trait Builds[-Elem, +To] extends Any { def build(xs: Foreach[Elem]): To }

/** Contravariance vs. implicits, the endless battle.
 *  We return a java three-value enum from compare in preference
 *  to a wild stab into the 2^32 states of an Int. This is a
 *  controversial thing to do, in the year 2014. Not joking.
 */
trait Order[-A] extends Any { def compare(x: A, y: A): Cmp }

/** When a 3-value enum isn't enough, try a 4-value one.
 */
trait PartialOrder[-A] extends Any { def partialCompare(x: A, y: A): PCmp }

/** Name-based extractor methods.
 */
trait IsEmpty extends Any                { def isEmpty: Boolean }
trait Opt[+A] extends Any with IsEmpty   { def get: A           }
trait OptInt extends Any with Opt[Int]   { def get: Int         }
trait OptLong extends Any with Opt[Long] { def get: Long        }

/** Collections interfaces.
 */
trait Foreach[+A]         extends Any with HasSize                        { def foreach(f: A => Unit): Unit }
trait Direct[+A]          extends Any with Foreach[A] with HasPreciseSize { def elemAt(i: Index): A         }
trait Intensional[-K, +V] extends Any                                     { def apply(x: K): V              }
trait Extensional[+A]     extends Any with HasSize                        { def contained: Foreach[A]       }

trait Linear[+A] extends Any with Foreach[A] with IsEmpty {
  def head: A
  def tail: Linear[A]
}

/** Ennhanced value representations.
 */
trait IndexRange extends Any with Direct[Index] {
  def start: Index
  def end: Index
}
trait Index extends Any with OptLong {
  def indexValue: Long
}

/** Generalized type constraint.
 */
sealed abstract class <:<[-From, +To] extends (From => To)
final class conformance[A] extends <:<[A, A] { def apply(x: A): A = x }
