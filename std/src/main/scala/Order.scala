package psp
package std

import Order._, Cmp._

/** Contravariance vs. implicits, the endless battle.
 *  We return a three value enum from compare in preference to
 *  a wild stab into the 2^32 states of an Int. This is a
 *  controversial thing to do, in the year 2014. Not joking.
 *  (I was joking about the enum though, since scala has no
 *  usable enum. We flounder along with a value class instead.)
 */
trait Order[-A] extends Any {
  def compare(x: A, y: A): Cmp
}

object Order {
  def apply[A](f: (A, A) => Cmp): OrderClass[A] = new OrderClass[A](f)

  implicit val intOrder     = apply[Int](_ - _ cmp)
  implicit val longOrder    = apply[Long](_ - _ cmp)
  implicit val charOrder    = apply[Char](_ - _ cmp)
  implicit val byteOrder    = apply[Byte](_ - _ cmp)
  implicit val shortOrder   = apply[Short](_ - _ cmp)
  implicit val booleanOrder = apply[Boolean](_ compare _ cmp)
  implicit val stringOrder  = apply[String](_ compareTo _ cmp)

  implicit def tuple2Order[A: Order, B: Order]           = apply[(A, B)]((x, y) => fold(x._1 compare y._1, x._2 compare y._2))
  implicit def tuple3Order[A: Order, B: Order, C: Order] = apply[(A, B, C)]((x, y) => fold(x._1 compare y._1, x._2 compare y._2, x._3 compare y._3))

  def fold(xs: Cmp*): Cmp = xs.toSeq findOr (_ != EQ, EQ)

  final class Cmp private (val intValue: Int) extends AnyVal {
    override def toString = this match {
      case LT => "LT"
      case EQ => "EQ"
      case GT => "GT"
    }
  }
  object Cmp {
    def apply(diff: Long): Cmp = if (diff < 0) LT else if (diff > 0) GT else EQ

    final val LT = new Cmp(-1)
    final val EQ = new Cmp(0)
    final val GT = new Cmp(1)
  }

  final class OrderClass[A](private val f: (A, A) => Cmp) extends AnyVal with Order[A] {
    def compare(x: A, y: A): Cmp = f(x, y)
  }
}
