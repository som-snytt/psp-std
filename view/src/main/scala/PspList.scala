package psp
package core

import psp.std._

object PspList {
  implicit def newBuilder[A] : Builds[A, PspList[A]] = Builds(fromForeach)

  def to(start: Int, end: Int): PspList[Int] = fromForeach(Foreach.to(start, end))

  // implicit def nilIsCovariant[A](xs: nil.type): PspList[A] = empty[A]
  def fromForeach[A](xs: Foreach[A]): PspList[A] = xs match {
    case xs: PspList[A] => xs
    case _              => xs.foldr(empty[A])(_ :: _).reverse
  }

  def empty[A] = nil.castTo[PspList[A]]
  def fill[A](n: Int)(body: => A): PspList[A] = if (n <= 0) nil() else body :: fill(n - 1)(body)
  def apply[A](xs: A*): PspList[A]            = xs.foldRight(nil[A]())(_ :: _)

  def ShowMax[A: Show](max: Int): Show[PspList[A]] = Show[PspList[A]] { xs =>
    val elems = xs take max
    val base = if (xs drop max isEmpty) "nil" else "..."

    if (elems.isEmpty) base else (elems join " :: ") + " :: nil"
  }
  implicit def ShowPspList[A: Show] : Show[PspList[A]] = Show[PspList[A]](xs =>
    if (xs.isEmpty) "nil" else show"${xs.head} :: ${xs.tail}"
  )
}

sealed trait PspList[A] extends LinearImpl[A] with InvariantLinear[A] {
  type Tail = PspList[A]
  def reverse: PspList[A] = {
    def loop(in: PspList[A], out: PspList[A]): PspList[A] = if (in.isEmpty) out else loop(in.tail, in.head :: out)
    loop(this, PspList.empty[A])
  }

  def take(n: Int): PspList[A] = {
    @tailrec def loop(n: Int, in: PspList[A], out: PspList[A]): PspList[A] =
      if (n <= 0 || in.isEmpty) out.reverse else loop(n - 1, in.tail, in.head :: out)

    loop(n, this, PspList[A]())
  }
  def drop(n: Int): PspList[A] = {
    @tailrec def loop(n: Int, xs: PspList[A]): PspList[A] = if (n <= 0 || xs.isEmpty) xs else loop(n - 1, xs.tail)
    loop(n, this)
  }

  def ?::[A1 >: A](x: A1): PspList[A1] = new ::[A1](x, this.asInstanceOf[PspList[A1]])
  def ::(x: A): PspList[A] = new ::(x, this)
  def :::(xs: PspList[A]): PspList[A] = xs match {
    case x :: xs => x :: (xs ::: this)
    case _       => this
  }
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
  def empty[A] = nil.castTo[InvariantLinear[A]]
  def cons[A](headFn: => A, tailFn: => InvariantLinear[A]): PspStream[A] = new PspStream[A](headFn, tailFn)

  def fromForeach[A](xs: Foreach[A]): InvariantLinear[A] = xs.foldr(empty[A])((x, res) => cons(x, res))
}
