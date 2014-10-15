package psp
package std

import api._

// Repr is the native representation, e.g. String, List[Int], Foreach[A].
sealed trait Walkable[-Repr] {
  type CC[X]                      // CC == collections class
  type A                          //  A == element type
  type VC[R] <: AtomicView[A, R]  // VC == view class

  def foreach(repr: Repr)(f: A => Unit): Unit
  def sizeInfo(repr: Repr): SizeInfo
  def wrap[R <: Repr](repr: R): VC[R]
}

trait Foreachable[-Repr] extends Walkable[Repr] {
  type VC[R] = AtomicView[A, R]

  def sizeInfo(repr: Repr): SizeInfo  = SizeInfo.unknown
  def wrap[R <: Repr](repr: R): VC[R] = new LinearView(repr, this)
}
trait DirectAccess[-Repr] extends Walkable[Repr] {
  type VC[R] = IndexedView[A, R]

  def length(repr: Repr): Precise
  def elemAt(repr: Repr)(i: Index): A
  def wrap[R <: Repr](repr: R): VC[R] = new IndexedView(repr, this, IndexRange(0, length(repr).sizeValue))
  def sizeInfo(repr: Repr): SizeInfo  = length(repr)
}

object Foreachable {
  final class PolicySetIs[AIn] extends Foreachable[PolicySet[AIn]] {
    type CC[X] = PolicySet[X]
    type A = AIn
    def foreach(repr: PolicySet[A])(f: A => Unit): Unit = repr foreach f
  }
  final class ForeachIs[AIn] extends Foreachable[Foreach[AIn]] {
    type CC[X] = Foreach[X]
    type A = AIn
    def foreach(repr: Foreach[A])(f: A => Unit): Unit = repr foreach f
  }
  final class TraversableIs[AIn] extends Foreachable[scTraversable[AIn]] {
    type CC[X] = scTraversable[X]
    type A = AIn
    def foreach(repr: CC[A])(f: A => Unit): Unit = repr foreach f
  }
  final class JavaIterableIs[AIn] extends Foreachable[jIterable[AIn]] {
    type CC[X] = jIterable[X]
    type A = AIn
    def foreach(repr: CC[A])(f: A => Unit): Unit = BiIterable(repr) foreach f
  }
  final class ArrayIs[AIn] extends Foreachable[Array[AIn]] {
    type CC[X] = Array[A]
    type A = AIn
    def foreach(repr: CC[A])(f: A => Unit): Unit = {
      val len = repr.length
      var i = 0
      while (i < len) {
        f(repr(i))
        i += 1
      }
    }
  }
}
object DirectAccess {
  trait Impl[AIn, Repr, M[X]] extends DirectAccess[Repr] {
    @inline final def foreach(repr: Repr)(f: A => Unit): Unit = length(repr) foreachIndex (i => f(elemAt(repr)(i)))
    type CC[X] = M[X]
    type A     = AIn
  }
  object StringIs extends Impl[Char, String, Direct] {
    def length(repr: String): Precise        = Precise(repr.length)
    def elemAt(repr: String)(i: Index): Char = repr charAt i.indexValue
  }
  final class ArrayIs[A] extends Impl[A, Array[A], Direct] {
    def length(repr: Array[A]): Precise     = Precise(repr.length)
    def elemAt(repr: Array[A])(i: Index): A = repr(i.indexValue)
  }
  final class ScalaIndexedIs[A] extends Impl[A, scIndexedSeq[A], scIndexedSeq] {
    def length(repr: CC[A]): Precise     = Precise(repr.length)
    def elemAt(repr: CC[A])(i: Index): A = repr(i.indexValue)
  }
  final class IndexedIs[A] extends Impl[A, Direct[A], Direct] {
    def length(repr: Direct[A]): Precise     = repr.size
    def elemAt(repr: Direct[A])(i: Index): A = repr elemAt i
  }
  final class IndexedViewIs[A, Repr] extends Impl[A, IndexedView[A, Repr], Direct] {
    def length(repr: IndexedView[A, Repr]): Precise     = repr.size
    def elemAt(repr: IndexedView[A, Repr])(i: Index): A = repr elemAt i
  }
}
