package psp
package core

import psp.std._

object PspList {
  implicit def newBuilder[A] : Builds[A, PspList[A]] = Builds(_.foldr(empty[A])(_ :: _).reverse)

  def empty[A] = nil.castTo[PspList[A]]
  def fill[A](n: Int)(body: => A): PspList[A] = if (n <= 0) nil() else body :: fill(n - 1)(body)
  def apply[A](xs: A*): PspList[A]            = xs.foldRight(nil[A]())(_ :: _)

  implicit def ShowPspList[A: Show] = Show[PspList[A]](xs => if (xs.isEmpty) "nil" else (xs join " :: ") + " :: nil")
}

sealed trait PspList[A] extends LinearImpl[A] with InvariantLinear[A] {
  type Tail = PspList[A]

  def reverse: PspList[A]      = reverser(this, nil())
  def take(n: Int): PspList[A] = taker(this, nil(), n)
  def drop(n: Int): PspList[A] = dropper(this, n)
  def ::(x: A): PspList[A]     = new ::(x, this)

  @tailrec private def reverser(in: PspList[A], out: PspList[A]): PspList[A] =
    if (in.isEmpty) out else reverser(in.tail, in.head :: out)

  @tailrec private def taker(in: PspList[A], out: PspList[A], n: Int): PspList[A] =
    if (n <= 0 || in.isEmpty) out.reverse else taker(in.tail, in.head :: out, n - 1)

  @tailrec private def dropper(xs: PspList[A], n: Int): PspList[A] =
    if (n <= 0 || xs.isEmpty) xs else dropper(xs.tail, n - 1)

}

final case object nil extends PspList[Nothing] {
  def isEmpty  = true
  def head     = failEmpty("head")
  def tail     = failEmpty("tail")

  def apply[A](): PspList[A] = this.castTo[PspList[A]]
  def unapply[A](xs: PspList[A]): Boolean = xs.isEmpty
}
final case class ::[A](head: A, tail: PspList[A]) extends PspList[A] {
  def isEmpty = false
}


final class PspStream[A](headFn: => A, tailFn: => InvariantLinear[A]) extends LinearImpl[A] with InvariantLinear[A] {
  type Tail = InvariantLinear[A]
  def isEmpty = false
  lazy val head = headFn
  lazy val tail = tailFn
}

object PspStream {
  def empty[A]                                                           = nil.castTo[InvariantLinear[A]]
  def cons[A](headFn: => A, tailFn: => InvariantLinear[A]): PspStream[A] = new PspStream[A](headFn, tailFn)
  def fromForeach[A](xs: Foreach[A]): InvariantLinear[A]                 = xs.foldr(empty[A])((x, res) => cons(x, res))
}
