package psp
package std

package api {
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
  trait Order[-A] extends Any { def compare(x: A, y: A): Cmp }

  object HashEq {
    // This converts Eq[A] to HashEq[A] assuming the built-in hashCode suffices.
    implicit def assumeHashCode[A](implicit eqs: Eq[A]): HashEq[A] = HashEq[A](eqs.equiv, _.##)
    def apply[A](cmp: (A, A) => Boolean, hashFn: A => Int): HashEq[A] = new internal.HashEqClass[A](cmp, hashFn)
  }

  object Eq {
    def apply[A](f: (A, A) => Boolean): internal.EqClass[A] = new internal.EqClass[A](f)

    implicit val booleanEq: Eq[Boolean] = Eq[Boolean](_ == _)
    implicit val byteEq: Eq[Byte]       = Eq[Byte](_ == _)
    implicit val charEq: Eq[Char]       = Eq[Char](_ == _)
    implicit val doubleEq: Eq[Double]   = Eq[Double](_ == _)
    implicit val floatEq: Eq[Float]     = Eq[Float](_ == _)
    implicit val intEq: Eq[Int]         = Eq[Int](_ == _)
    implicit val longEq: Eq[Long]       = Eq[Long](_ == _)
    implicit val shortEq: Eq[Short]     = Eq[Short](_ == _)
    implicit val stringEq: Eq[String]   = Eq[String](_ == _)
    implicit val unitEq: Eq[Unit]       = Eq[Unit]((x, y) => true)
  }
}

package internal {
  import api._

  /** The funny parameter names are because they can't be made private in 2.10
   *  due to value class limitations, but that leaves them eligible to drive
   *  implicit conversions to these classes.
   */
  final class EqClass[-A](val __psp_f: (A, A) => Boolean) extends AnyVal with Eq[A] {
    def equiv(x: A, y: A): Boolean = __psp_f(x, y)
  }
  final class OrderClass[-A](val __psp_f: (A, A) => Cmp) extends AnyVal with Order[A] {
    def compare(x: A, y: A): Cmp = __psp_f(x, y)
  }
  final class ShowClass[-A](val __psp_f: A => String) extends AnyVal with Show[A] {
    def show(x: A): String = __psp_f(x)
  }
  final class ReadClass[A](val __psp_f: String => A) extends AnyVal with Read[A] {
    def read(x: String): A = __psp_f(x)
  }
  final class HashEqClass[-A](cmp: (A, A) => Boolean, h: A => Int) extends HashEq[A] {
    def equiv(x: A, y: A) = cmp(x, y)
    def hash(x: A)        = h(x)
  }
}
