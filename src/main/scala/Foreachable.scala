package psp
package core

import impl._

trait Foreachable[-Repr] {
  type A
  type CC[X]
  type Coll = CC[A]

  def convert(repr: Repr): CC[A]
  def foreach(repr: Repr)(f: A => Unit): Unit
}

trait Indexable[-Repr] extends Foreachable[Repr] {
  def convert(repr: Repr): CC[A]
  def length(repr: Repr): Size
  def elemAt(repr: Repr)(index: Index): A
  def foreach(repr: Repr)(f: A => Unit): Unit = length(repr).toInterval foreach (i => f(elemAt(repr)(i)))
}

trait ForeachableSet[-Repr] extends Foreachable[Repr] {
  def contains(xs: Repr)(elem: A): Boolean
}

object Indexable {
  implicit def stringIsIndexable: StringIsIndexable.type                                       = StringIsIndexable
  implicit def arrayIsIndexable[@specialized A] : ArrayIsIndexable[A]                          = new ArrayIsIndexable[A]
  implicit def indexedSeqIsIndexable[CC[X] <: IndexedSeq[X], A] : IndexedSeqIsIndexable[CC, A] = new IndexedSeqIsIndexable[CC, A]
  implicit def pspIndexedIsIndexable[A] : PspIndexedIsIndexable[A]                             = new PspIndexedIsIndexable[A]
}

object Foreachable {
  implicit def pspViewableIsForeachable[A] : PspViewableIsForeachable[A]                                        = new PspViewableIsForeachable[A]
  implicit def pspForeachIsForeachable[A] : PspForeachIsForeachable[A]                                          = new PspForeachIsForeachable[A]
  implicit def scalaTraversableIsForeachable[CC[X] <: Traversable[X], A] : ScalaTraversableIsForeachable[CC, A] = new ScalaTraversableIsForeachable[CC, A]
}
