package psp
package std
package api

/** A trait containing implicits to ease scala compatibility,
 *  but with some risk of creating ambiguity or masking errors.
 *  These aren't included by default.
 */
trait ScalaCompat extends Any with LowPriorityScalaCompat {
  implicit def orderToOrdering[A](implicit ord: Order[A]): Ordering[A] =
    new Ordering[A] { def compare(x: A, y: A): Int = ord.compare(x, y).intValue }
}

trait LowPriorityScalaCompat extends Any {
  implicit def showToOrdering[A](implicit show: Show[A]): Ordering[A] =
    new Ordering[A] { def compare(x: A, y: A): Int = implicitly[Ordering[String]].compare(show show x, show show y) }
}
