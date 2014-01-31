package psp
package core

import impl._

sealed trait Walkable[-Repr] {
  type A
  type CC[X]
  type Coll = CC[A]

  def foreach(repr: Repr)(f: A => Unit): Unit
}

trait Foreachable[-Repr] extends Walkable[Repr] {
  def sizeInfo(repr: Repr): SizeInfo = SizeInfo.Unknown
  def wrap[R <: Repr](repr: R): AtomicView[R, CC, A] = AtomicView.unknown(repr)(this)
}

trait Linearable[Repr] extends Walkable[Repr] {
  def head(repr: Repr): A
  def tail(repr: Repr): Repr
  def isEmpty(repr: Repr): Boolean

  def wrap(repr: Repr): LinearView[Repr, CC, A] = AtomicView.linear(repr)(this)
}

trait Indexable[-Repr] extends Walkable[Repr] {
  def length(repr: Repr): Size
  def elemAt(repr: Repr)(index: Index): A
  def foreach(repr: Repr)(f: A => Unit): Unit = length(repr).toInterval foreach (i => f(elemAt(repr)(i)))

  def wrap[R <: Repr](repr: R): IndexedView[R, CC, A] = AtomicView.indexed(repr)(this)
}

object Linearable {
  implicit def pspLinearIsLinearable[A] : PspLinearIsLinearable[A] = new PspLinearIsLinearable[A]
}
object Foreachable {
  implicit def pspForeachIsForeachable[A] : PspForeachIsForeachable[A] = new PspForeachIsForeachable[A]

  implicit def traversableIsForeachable[CC[X] <: Traversable[X], A] : TraversableIsForeachable[CC, A] = new TraversableIsForeachable[CC, A]
}
object Indexable {
  implicit def pspIndexedIsIndexable[A] : PspIndexedIsIndexable[A] = new PspIndexedIsIndexable[A]

  implicit def stringIsIndexable: StringIsIndexable.type                                                                           = StringIsIndexable
  implicit def arrayIsIndexable[A: ClassTag] : ArrayIsIndexable[A]                                                                 = new ArrayIsIndexable[A]
  implicit def indexedSeqIsIndexable[CC[X] <: IndexedSeq[X], A](implicit pcb: PspCanBuild[A, CC[A]]): IndexedSeqIsIndexable[CC, A] = new IndexedSeqIsIndexable[CC, A]
}
