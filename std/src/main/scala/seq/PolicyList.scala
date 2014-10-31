package psp
package std

import api._

sealed trait PolicyList[A] extends Linear[A]
final case object pNil extends PolicyList[Nothing] {
  def foreach(f: Nothing => Unit): Unit = ()
  def size = Size.Empty
  def isEmpty  = true
  def head     = abort("pNil.head")
  def tail     = abort("pNil.tail")
  override def toString = "nil"
}
final case class pCons[A](head: A, tail: PolicyList[A]) extends PolicyList[A] {
  def foreach(f: A => Unit): Unit = { f(head) ; tail foreach f }
  def size = Size.NonEmpty
  def isEmpty  = false
  override def toString = s"$head :: $tail"
}

final class pChunked[A](private val chunks: Linear[Linear[A]]) extends Linear[A] {
  private def nonEmptyHead: pChunked[A] = if (chunks.isEmpty || chunks.head.nonEmpty) this else new pChunked(chunks.tail).nonEmptyHead

  def isEmpty  = nonEmptyHead.chunks.isEmpty
  def head     = nonEmptyHead.head
  def tail     = nonEmptyHead.tail
  def size = if (isEmpty) Size.Empty else Size.NonEmpty
  def foreach(f: A => Unit): Unit = { f(head) ; tail foreach f }
}

object PolicyList {
  implicit class PolicyListOps[A](val xs: PolicyList[A]) extends AnyVal {
    def ::(x: A): pCons[A] = new pCons(x, xs)
  }
  /** FIXME - This should be a value class but in 2.10 it's our friend Mr. Bug again.
      [error] (run-main-0) java.lang.ClassCastException: psp.std.PolicyList$FromScala cannot be cast to scala.collection.immutable.LinearSeq
      java.lang.ClassCastException: psp.std.PolicyList$FromScala cannot be cast to scala.collection.immutable.LinearSeq
        at psp.std.package$.fromScala(package.scala:210)
        at psp.std.package$.fromElems(package.scala:221)
        at psp.std.PolicySet$.elems(PolicySet.scala:13)
   */
  final class FromScala[A](val xs: sciLinearSeq[A]) extends AnyRef with Linear[A] {
    def size    = Size(xs)
    def isEmpty = xs.isEmpty
    def head    = xs.head
    def tail    = new FromScala(xs.tail)
    def foreach(f: A => Unit) = xs foreach f
  }

  def builder[A] : Builds[A, PolicyList[A]]      = Builds(xs => xs.foldr(empty[A])(_ :: _))
  def empty[A] : PolicyList[A]                   = pNil.castTo
  def fill[A](n: Int)(body: => A): PolicyList[A] = if (n <= 0) empty[A] else body :: fill(n - 1)(body)

  // Length check necessary to avoid infinite recursion in foldr, which may utilize this class
  def apply[A](xs: A*): PolicyList[A] = if (xs.length == 0) empty[A] else xs.m.foldr(empty[A])(_ :: _)
}

/** A wrapper so a fixed-size sequence can linearly be decomposed with a bit less
 *  allocation overhead.
 */
final class Linearized[A](xs: Direct[A], startIndex: Index) extends Linear[A] with HasPreciseSize {
  def foreach(f: A => Unit): Unit = xs.indices drop startIndex.indexValue.size foreach (i => f(xs(i)))

  def size    = xs.size - startIndex.indexValue.size
  def isEmpty = startIndex.isUndefined || xs.lastIndex < startIndex
  def head    = xs(startIndex)
  def tail    = new Linearized(xs, startIndex.next)
}
