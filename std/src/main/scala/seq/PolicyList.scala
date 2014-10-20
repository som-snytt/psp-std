package psp
package std

import api._

sealed trait PolicyList[A] extends Linear[A]
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

final class pChunked[A](private val chunks: Linear[Linear[A]]) extends Linear[A] {
  private def nonEmptyHead: pChunked[A] = if (chunks.isEmpty || chunks.head.nonEmpty) this else new pChunked(chunks.tail).nonEmptyHead

  def isEmpty  = nonEmptyHead.chunks.isEmpty
  def head     = nonEmptyHead.head
  def tail     = nonEmptyHead.tail
  def sizeInfo = if (isEmpty) SizeInfo.Empty else SizeInfo.NonEmpty
  def foreach(f: A => Unit): Unit = { f(head) ; tail foreach f }
}

object PolicyList {
  implicit class PolicyListOps[A](val xs: PolicyList[A]) extends AnyVal {
    def ::(x: A): pCons[A] = new pCons(x, xs)
  }
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

/** A wrapper so a fixed-size sequence can linearly be decomposed with a bit less
 *  allocation overhead.
 */
final class Linearized[A](xs: pVector[A], startIndex: Index) extends Linear[A] with HasPreciseSize {
  def foreach(f: A => Unit): Unit = xs.indices drop startIndex.indexValue.size foreach (i => f(xs(i)))

  def sizeInfo = xs.sizeInfo - startIndex.indexValue.size
  def isEmpty  = startIndex.isUndefined || xs.lastIndex < startIndex
  def head     = xs(startIndex)
  def tail     = new Linearized(xs, startIndex.next)
}
