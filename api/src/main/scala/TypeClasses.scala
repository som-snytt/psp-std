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

trait TryShow[-A] extends Any { def show(x: A): String }

/** Contravariance vs. implicits, the endless battle.
 *  We return a java three-value enum from compare in preference
 *  to a wild stab into the 2^32 states of an Int. This is a
 *  controversial thing to do, in the year 2014. Not joking.
 */
trait Order[-A] extends Any { def compare(x: A, y: A): Cmp }

trait OrderEq[-A] extends Any with Order[A] with Eq[A] {
  def equiv(x: A, y: A): Boolean = compare(x, y) == Cmp.EQ
}
trait PartialOrderEq[-A] extends Any with PartialOrder[A] with Eq[A] {
  def equiv(x: A, y: A): Boolean = partialCompare(x, y) == PCmp.EQ
}

trait PartialOrder[-A] extends Any {
  def partialCompare(lhs: A, rhs: A): PCmp
  def tryCompare(lhs: A, rhs: A): Option[Cmp] = partialCompare(lhs, rhs) match {
    case PCmp.LT => Some(Cmp.LT)
    case PCmp.GT => Some(Cmp.GT)
    case PCmp.EQ => Some(Cmp.EQ)
    case _       => None
  }
}

object Ops {
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
  final class OrderEqClass[-A](val __psp_f: (A, A) => Cmp) extends AnyVal with OrderEq[A] {
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

  final class OrderOps[A](val __psp_ord: Order[A]) extends AnyVal {
    def reverse: Order[A]          = Order[A]((x, y) => __psp_ord.compare(x, y).flip)
    def toOrdering: Ordering[A]    = newOrdering[A](__psp_ord.compare)
    def on[B](f: B => A): Order[B] = Order[B]((x, y) => __psp_ord.compare(f(x), f(y)))
  }

  /** Working around 2.10 value class bug. */
  private def newOrdering[A](f: (A, A) => Cmp): Ordering[A] =
    new Ordering[A] { def compare(x: A, y: A): Int = f(x, y).intValue }
}

object HashEq {
  // This converts Eq[A] to HashEq[A] assuming the built-in hashCode suffices.
  implicit def assumeHashCode[A](implicit eqs: Eq[A]): HashEq[A] = HashEq[A](eqs.equiv, _.##)
  def apply[A](cmp: (A, A) => Boolean, hashFn: A => Int): HashEq[A] = new Ops.HashEqClass[A](cmp, hashFn)
}

object Read {
  def apply[A](f: String => A): Read[A] = new Ops.ReadClass[A](f)
}
object Order {
  def apply[A](f: (A, A) => Cmp): Order[A] = new Ops.OrderClass[A](f)
}
object OrderEq {
  def apply[A](f: (A, A) => Cmp): OrderEq[A] = new Ops.OrderEqClass[A](f)
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

trait LowTryShow {
  self: TryShow.type =>

  implicit def hasNoShow[A] : TryShow[A] = NoShow
}
object TryShow extends LowTryShow {
  final class HasShow[-A](shows: Show[A]) extends TryShow[A] { def show(x: A) = shows show x }
  final object NoShow extends TryShow[Any] { def show(x: Any): String = "" + x }

  implicit def hasShow[A](implicit shows: Show[A]): HasShow[A] = new HasShow(shows)
}
