package psp
package std
package api

/** A trait containing implicits to ease scala compatibility,
 *  but with some risk of creating ambiguity or masking errors.
 *  These aren't included by default.
 */
trait ScalaCompat extends LowPriorityScalaCompat {
  implicit def orderToOrdering[A](implicit ord: Order[A]): Ordering[A] =
    new Ordering[A] { def compare(x: A, y: A): Int = ord.compare(x, y).intValue }
}

trait LowPriorityScalaCompat {
  implicit def showToOrdering[A](implicit show: Show[A]): Ordering[A] =
    new Ordering[A] { def compare(x: A, y: A): Int = (show show x) compare (show show y) }
}
