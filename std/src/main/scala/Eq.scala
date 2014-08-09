package psp
package std

/** The classic type class for encoding value equivalence.
 */
trait Eq[A] {
  def equiv(x: A, y: A): Boolean
}
object Eq {
  def apply[A](f: (A, A) => Boolean): Eq[A] = new Eq[A] { def equiv(x: A, y: A) = f(x, y) }

  implicit def stringEq: Eq[String]                  = Eq[String](_ == _)
  implicit def seqEq[CC[X] <: Seq[X], A] : Eq[CC[A]] = Eq[CC[A]](_ sameElements _)
  implicit def arrayEq[A] : Eq[Array[A]]             = Eq[Array[A]](_.toSeq sameElements _.toSeq)
  implicit def anyValEq[A <: AnyVal] : Eq[A]         = Eq[A](_ == _)

  final class Ops[A](val x: A) extends AnyVal {
    def ===(that: A)(implicit eq: Eq[A]): Boolean = eq.equiv(x, that)
  }
}
