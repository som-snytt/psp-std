package psp
package core

import impl._
import psp.std._

trait WalkableTypes {
  type A
  type CC[X]
}

sealed trait Walkable[-Repr] extends WalkableTypes {
  def foreach(repr: Repr)(f: A => Unit): Unit
}

trait Foreachable[-Repr] extends Walkable[Repr] {
  def sizeInfo(repr: Repr): SizeInfo = unknownSize
  def wrap[R <: Repr](repr: R): AtomicView[R, this.type] = AtomicView.unknown(repr)(this)
}

trait SequentialAccess[Repr] extends Walkable[Repr] {
  def head(repr: Repr): A
  def tail(repr: Repr): Repr
  def isEmpty(repr: Repr): Boolean

  def wrap(repr: Repr): LinearView[Repr, this.type] = AtomicView.linear(repr)(this)
}

trait DirectAccess[-Repr] extends Walkable[Repr] {
  def length(repr: Repr): Size
  def elemAt(repr: Repr)(index: Index): A

  def wrap[R <: Repr](repr: R): IndexedView[R, this.type] = AtomicView.indexed(repr)(this)
}

object SequentialAccess {
  implicit def pspLinearIsSequentialAccess[A] : PspLinearIsSequentialAccess[A] = new PspLinearIsSequentialAccess[A]
}
object Foreachable {
  implicit def pspForeachIsForeachable[A] : PspForeachIsForeachable[A] = new PspForeachIsForeachable[A]

  implicit def traversableIsForeachable[CC[X] <: Traversable[X], A] : TraversableIsForeachable[CC, A] = new TraversableIsForeachable[CC, A]
}
object DirectAccess {
  implicit def pspIndexedIsDirectAccess[A] : PspIndexedIsDirectAccess[A] = new PspIndexedIsDirectAccess[A]

  implicit def stringIsCharSequence: StringIsCharSequence.type = StringIsCharSequence

  implicit def arrayIsDirectAccess[A: ClassTag] : ArrayIsDirectAccess[A]                                                                 = new ArrayIsDirectAccess[A]
  implicit def indexedSeqIsDirectAccess[CC[X] <: IndexedSeq[X], A](implicit pcb: Builds[A, CC[A]]): IndexedSeqIsDirectAccess[CC, A] = new IndexedSeqIsDirectAccess[CC, A]
}
