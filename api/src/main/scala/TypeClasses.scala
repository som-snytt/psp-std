package psp
package std
package api

/** The classic type class for encoding value equivalence.
 */
trait Eq[-A] extends Any { def equiv(x: A, y: A): Boolean }

/** The Eq type class fused with a method to provide the
 *  corresponding hashCodes. I've never had a desire to provide
 *  hash codes independently of equals logic so there's no
 *  separate Hash typeclass.
 */
trait HashEq[-A] extends Any with Eq[A] { def hash(x: A): Int }

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
