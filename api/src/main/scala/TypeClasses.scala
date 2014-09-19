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

  sealed trait TryShow[-A] extends Any { def show(x: A): String }
  final class HasShow[-A](shows: Show[A]) extends TryShow[A] { def show(x: A) = shows show x }
  final object HasNoShow extends TryShow[Any] { def show(x: Any): String = "" + x }

  /** Contravariance vs. implicits, the endless battle.
   *  We return a java three-value enum from compare in preference
   *  to a wild stab into the 2^32 states of an Int. This is a
   *  controversial thing to do, in the year 2014. Not joking.
   */
  trait Order[-A] extends Any with Eq[A] {
    def compare(x: A, y: A): Cmp
    def equiv(x: A, y: A): Boolean = compare(x, y) == Cmp.EQ
  }

  trait PartialOrder[-A] extends Any with Eq[A] {
    def tryCompare(lhs: A, rhs: A): Option[Cmp] = partialCompare(lhs, rhs) match {
      case PCmp.LT => Some(Cmp.LT)
      case PCmp.GT => Some(Cmp.GT)
      case PCmp.EQ => Some(Cmp.EQ)
      case _       => None
    }
    def partialCompare(lhs: A, rhs: A): PCmp
    def equiv(x: A, y: A): Boolean = partialCompare(x, y) == PCmp.EQ
  }

  object HashEq {
    // This converts Eq[A] to HashEq[A] assuming the built-in hashCode suffices.
    implicit def assumeHashCode[A](implicit eqs: Eq[A]): HashEq[A] = HashEq[A](eqs.equiv, _.##)
    def apply[A](cmp: (A, A) => Boolean, hashFn: A => Int): HashEq[A] = new Ops.HashEqClass[A](cmp, hashFn)
  }

  object Eq {
    def native[A](): Eq[A]                             = Eq[A](_ == _)
    def apply[A](f: (A, A) => Boolean): Ops.EqClass[A] = new Ops.EqClass[A](f)

    implicit val booleanEq: Eq[Boolean] = native[Boolean]
    implicit val byteEq: Eq[Byte]       = native[Byte]
    implicit val charEq: Eq[Char]       = native[Char]
    implicit val doubleEq: Eq[Double]   = native[Double]
    implicit val floatEq: Eq[Float]     = native[Float]
    implicit val intEq: Eq[Int]         = native[Int]
    implicit val longEq: Eq[Long]       = native[Long]
    implicit val shortEq: Eq[Short]     = native[Short]
    implicit val stringEq: Eq[String]   = native[String]
    implicit val unitEq: Eq[Unit]       = Eq[Unit]((x, y) => true)
  }

  object Read {
    def apply[A](f: String => A): Read[A] = new Ops.ReadClass[A](f)
  }
  object Show {
    def apply[A](f: A => String): Show[A] = new Ops.ShowClass[A](f)
    def native[A](): Show[A]              = ToString

    /** This of course is not implicit as that would defeat the purpose of the endeavor.
     */
    private val ToString: Show[Any] = apply[Any] {
      case x: ShowDirect => x.to_s
      case x             => "" + x
    }
  }
  trait LowTryShow {
    implicit def hasNoShow[A] : TryShow[A] = HasNoShow
  }
  object TryShow extends LowTryShow {
    implicit def hasShow[A](implicit shows: Show[A]): HasShow[A] = new HasShow(shows)
  }
  object Order {
    def apply[A](f: (A, A) => Cmp): Order[A] = new Ops.OrderClass[A](f)
  }
}
