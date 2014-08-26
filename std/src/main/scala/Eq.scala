package psp
package std

/** The classic type class for encoding value equivalence.
 */
trait Eq[A] extends Any {
  def equiv(x: A, y: A): Boolean
}
trait HashEq[A] extends Any with Eq[A] {
  def hash(x: A): Int
}

object HashEq {
  implicit def assumeHashCode[A](implicit eqs: Eq[A]): HashEq[A] = apply[A](eqs.equiv, _.##)

  def universal[A]           = apply[A](_ == _, _.##)
  def reference[A <: AnyRef] = apply[A](_ eq _, System.identityHashCode)
  def shown[A: Show]         = apply[A](_.to_s == _.to_s, _.to_s.##)

  def apply[A](cmp: (A, A) => Boolean, hashFn: A => Int): HashEq[A] = new HashEq[A] {
    def equiv(x: A, y: A) = cmp(x, y)
    def hash(x: A)        = hashFn(x)
  }
}

object Eq {
  def apply[A](f: (A, A) => Boolean): EqClass[A] = new EqClass[A](f)

  implicit val stringEq  = Eq[String](_ == _)
  implicit val byteEq    = Eq[Byte](_ == _)
  implicit val shortEq   = Eq[Short](_ == _)
  implicit val charEq    = Eq[Char](_ == _)
  implicit val intEq     = Eq[Int](_ == _)
  implicit val longEq    = Eq[Long](_ == _)
  implicit val floatEq   = Eq[Float](_ == _)
  implicit val doubleEq  = Eq[Double](_ == _)
  implicit val booleanEq = Eq[Boolean](_ == _)
  implicit val unitEq    = Eq[Unit]((x, y) => true)

  implicit def mapEq[CC[X, Y] <: Map[X, Y], K: Eq, V: Eq] : Eq[CC[K, V]] = Eq[CC[K, V]]((xs, ys) => each(xs.keys).toSet === each(ys.keys).toSet && xs.keys.forall(k => xs(k) === ys(k)))
  implicit def setEq[CC[X] <: Set[X], A: HashEq] : Eq[CC[A]]             = Eq[CC[A]]((xs, ys) => each(xs).toSet === each(ys).toSet)
  implicit def seqEq[CC[X] <: Seq[X], A: Eq] : Eq[CC[A]]                 = Eq[CC[A]]((xs, ys) => (xs corresponds ys)(_ === _))
  implicit def arrayEq[A: Eq] : Eq[Array[A]]                             = Eq[Array[A]](_.toSeq == _.toSeq)

  final class EqClass[A](private val f: (A, A) => Boolean) extends AnyVal with Eq[A] {
    def equiv(x: A, y: A): Boolean = f(x, y)
  }
  final case class Wrap[A: HashEq](val value: A) {
    override def hashCode = ?[HashEq[A]] hash value
    override def equals(x: Any): Boolean = x match {
      case Wrap(that) => value === that.asInstanceOf[A]
      case _          => false
    }
    override def toString = pp"$value"
  }
}

class OrderBy[A] { def apply[B](f: A => B)(implicit ord: Order[B]): Order[A] = ord on f   }
class EqBy[A]    { def apply[B](f: A => B)(implicit equiv: Eq[B]): Eq[A]     = equiv on f }
class ShowBy[A]  { def apply[B](f: A => B)(implicit show: Show[B]): Show[A]  = show on f  }
