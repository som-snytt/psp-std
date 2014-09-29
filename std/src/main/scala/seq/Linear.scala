package psp
package std
package linear

import SizeInfo._

trait Leaf[A] extends Any with api.Linear[A] {
  def sizeInfo = if (isEmpty) Empty else NonEmpty
  @inline final def foreach(f: A => Unit): Unit = {
    @tailrec def loop(xs: api.Linear[A]): Unit = if (!xs.isEmpty) { f(xs.head) ; loop(xs.tail) }
    loop(this)
  }
  def contains(x: A): Boolean = {
    @tailrec def loop(xs: api.Linear[A]): Boolean = !xs.isEmpty && (x == xs.head || loop(xs.tail))
    loop(this)
  }
}

object List {
  def empty[A]                             = Nil.castTo[List[A]]
  def fill[A](n: Int)(body: => A): List[A] = if (n <= 0) Nil() else body :: fill(n - 1)(body)
  def apply[A](xs: A*): List[A]            = xs.foldRight(Nil[A]())(_ :: _)
}

sealed trait List[A] extends Leaf[A] {
  type Tail                    = List[A]
  def reverse: List[A]      = reverser(this, Nil())
  def take(n: Int): List[A] = taker(this, Nil(), n)
  def drop(n: Int): List[A] = dropper(this, n)
  def ::(x: A): List[A]     = new ::(x, this)

  @tailrec private def reverser(in: List[A], out: List[A]): List[A] =
    if (in.isEmpty) out else reverser(in.tail, in.head :: out)

  @tailrec private def taker(in: List[A], out: List[A], n: Int): List[A] =
    if (n <= 0 || in.isEmpty) out.reverse else taker(in.tail, in.head :: out, n - 1)

  @tailrec private def dropper(xs: List[A], n: Int): List[A] =
    if (n <= 0 || xs.isEmpty) xs else dropper(xs.tail, n - 1)
}

final case object Nil extends List[Nothing] {
  def isEmpty  = true
  def head     = failEmpty("head")
  def tail     = failEmpty("tail")

  def apply[A](): List[A] = this.castTo[List[A]]
  def unapply[A](xs: List[A]): Boolean = xs.isEmpty
}
final case class ::[A](head: A, tail: List[A]) extends List[A] {
  def isEmpty = false
}


final class Stream[A](headFn: => A, tailFn: => Leaf[A]) extends Leaf[A] {
  type Tail = Leaf[A]
  def isEmpty = false
  lazy val head = headFn
  lazy val tail = tailFn
}

object Stream {
  def empty[A]                                                      = Nil.castTo[Leaf[A]]
  def cons[A](headFn: => A, tailFn: => Leaf[A]): Stream[A] = new Stream[A](headFn, tailFn)
}
