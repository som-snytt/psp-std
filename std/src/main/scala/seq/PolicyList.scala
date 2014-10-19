package psp
package std

import api._

sealed trait PolicyList[A] extends Linear[A] {
  def ::(x: A): PolicyList[A] = new pCons(x, this)
}
final case object pNil extends PolicyList[Nothing] {
  def foreach(f: Nothing => Unit): Unit = ()
  def sizeInfo = SizeInfo.Empty
  def isEmpty  = true
  def head     = abort("pNil.head")
  def tail     = abort("pNil.tail")
}
final case class pCons[A](head: A, tail: PolicyList[A]) extends PolicyList[A] {
  def foreach(f: A => Unit): Unit = { f(head) ; tail foreach f }
  def sizeInfo = SizeInfo.NonEmpty
  def isEmpty  = false
}
object PolicyList {
  final class FromScala[A](val xs: sciLinearSeq[A]) extends AnyVal with Linear[A] {
    def sizeInfo = SizeInfo(xs)
    def isEmpty  = xs.isEmpty
    def head     = xs.head
    def tail     = new FromScala(xs.tail)
    def foreach(f: A => Unit) = xs foreach f
  }

  def builder[A] : Builds[A, PolicyList[A]]      = Builds(xs => xs.foldr(empty[A])(_ :: _))
  def empty[A] : PolicyList[A]                   = pNil.castTo
  def fill[A](n: Int)(body: => A): PolicyList[A] = if (n <= 0) empty[A] else body :: fill(n - 1)(body)
  def apply[A](xs: A*): PolicyList[A]            = xs.seq.pvec.foldr(empty[A])(_ :: _)
}
