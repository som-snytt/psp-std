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
    def apply[A](cmp: (A, A) => Boolean, hashFn: A => Int): HashEq[A] = new Ops.HashEqClass[A](cmp, hashFn)
  }

  object Eq {
    def apply[A](f: (A, A) => Boolean): Ops.EqClass[A] = new Ops.EqClass[A](f)

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
  object Order {
    def apply[A](f: (A, A) => Cmp): Order[A] = new Ops.OrderClass[A](f)
  }
}
