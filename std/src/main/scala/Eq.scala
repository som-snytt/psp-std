package psp
package std

/** The classic type class for encoding value equivalence.
 */
trait Eq[A] {
  def equiv(x: A, y: A): Boolean
}
object Eq {
  def by[A] : By[A] = new By[A]

  def apply[A](f: (A, A) => Boolean): Eq[A] = new Eq[A] { def equiv(x: A, y: A) = f(x, y) }

  implicit def stringEq: Eq[String]                      = Eq[String](_ == _)
  implicit def seqEq[CC[X] <: Seq[X], A: Eq] : Eq[CC[A]] = Eq[CC[A]]((xs, ys) => (xs corresponds ys)(_ === _))
  implicit def arrayEq[A: Eq] : Eq[Array[A]]             = Eq[Array[A]](_.toSeq === _.toSeq)
  implicit def anyValEq[A <: AnyVal] : Eq[A]             = Eq[A](_ == _)

  final class Ops[A](val x: A) extends AnyVal {
    def ===(that: A)(implicit eq: Eq[A]): Boolean = eq.equiv(x, that)
    def !==(that: A)(implicit eq: Eq[A]): Boolean = !(this === that)
  }

  final class By[A] {
    def apply[B](f: A => B)(implicit eqs: Eq[B]): Eq[A] = eqs on f
  }
  final class EqClass[A](f: (A, A) => Boolean) extends Eq[A] {
    def equiv(x: A, y: A) = f(x, y)
  }
  final class EqOps[A](val eqs: Eq[A]) extends AnyVal {
    def on[B](f: B => A): Eq[B] = new EqClass[B]((x, y) => eqs.equiv(f(x), f(y)))
  }
}
