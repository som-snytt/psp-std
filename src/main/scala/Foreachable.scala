package psp
package core

import impl._

trait Foreachable[-Repr] {
  type A
  type CC[X]
  type Coll = CC[A]
  def foreach(repr: Repr)(f: A => Unit): Unit
  def wrap[R <: Repr](repr: R): AtomicView[R, CC, A] = AtomicView.linear(repr)(this)
}

trait Indexable[-Repr] extends Foreachable[Repr] {
  def length(repr: Repr): Size
  def elemAt(repr: Repr)(index: Index): A
  def foreach(repr: Repr)(f: A => Unit): Unit = length(repr).toInterval foreach (i => f(elemAt(repr)(i)))
  override def wrap[R <: Repr](repr: R): IndexedView[R, CC, A] = AtomicView.indexed(repr)(this)
}

object Foreachable {
  implicit def pspForeachIsForeachable[A] : PspForeachIsForeachable[A]                                          = new PspForeachIsForeachable[A]
  implicit def scalaTraversableIsForeachable[CC[X] <: Traversable[X], A] : ScalaTraversableIsForeachable[CC, A] = new ScalaTraversableIsForeachable[CC, A]
}
object Indexable {
  implicit def stringIsIndexable: StringIsIndexable.type                                                                           = StringIsIndexable
  implicit def arrayIsIndexable[A: ClassTag] : ArrayIsIndexable[A]                                                                 = new ArrayIsIndexable[A]
  implicit def indexedSeqIsIndexable[CC[X] <: IndexedSeq[X], A](implicit pcb: PspCanBuild[A, CC[A]]): IndexedSeqIsIndexable[CC, A] = new IndexedSeqIsIndexable[CC, A]
  implicit def pspIndexedIsIndexable[A] : PspIndexedIsIndexable[A]                                                                 = new PspIndexedIsIndexable[A]
}
