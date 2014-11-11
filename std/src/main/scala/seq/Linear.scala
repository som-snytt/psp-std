package psp
package std

import api._

object Linear {
  final val Empty: Linear[Nothing] = WrapList(Nil)

  final case class WrapStream[A](xs: sciStream[A]) extends AnyVal with Linear[A] {
    def ::(x: A): WrapStream[A] = WrapStream(sciStream.cons(x, xs))

    def isEmpty                 = xs.isEmpty
    def size: Size              = if (isEmpty) Size.Empty else Size.NonEmpty
    def head: A                 = xs.head
    def tail: WrapStream[A]     = WrapStream(xs.tail)

    @inline def foreach(f: A => Unit): Unit = xs foreach f
  }

  final case class WrapList[A](xs: sciList[A]) extends AnyVal with Linear[A] {
    def ::(x: A): WrapList[A] = WrapList(x :: xs)

    def isEmpty           = xs.isEmpty
    def size: Size        = if (isEmpty) Size.Empty else Size.NonEmpty
    def head: A           = xs.head
    def tail: WrapList[A] = WrapList(xs.tail)
    @inline def foreach(f: A => Unit): Unit = xs foreach f
  }

  def fromScala[A](xs: sCollection[A]): Linear[A] = xs match {
    case xs: sciStream[A] => WrapStream(xs)
    case _                => WrapList(xs.toList)
  }
  def fromJava[A](xs: jIterable[A]): Linear[A]    = WrapList(xs.m.toScalaList)

  def builder[A] : Builds[A, Linear[A]]        = Builds[A, sciList[A]](_.toScalaList) map fromScala
  def empty[A] : Linear[A]                     = Empty
  def fill[A](n: Int)(body: => A): WrapList[A] = WrapList[A](sciList.fill(n)(body))
  def apply[A](xs: A*): WrapList[A]            = WrapList[A](xs.toList)
}
