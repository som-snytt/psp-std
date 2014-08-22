package psp
package core
package impl

import scala.{ collection => sc }
import sc.{ mutable => scm }
import psp.std._

/** Compat
 */

final class TraversableAsForeach[+A](underlying: Traversable[A]) extends Foreach[A] {
  def sizeInfo: SizeInfo          = SizeInfo(underlying)
  def foreach(f: A => Unit): Unit = underlying foreach f
  override def toString           = underlying.shortClass + " (wrapped)"
}

final class ForeachAsTraversable[+A](underlying: Foreach[A]) extends sc.immutable.Traversable[A] {
  def foreach[U](f: A => U): Unit = underlying foreach (x => f(x))
}

/** Direct
 */

abstract class IndexedImpl[+A](val size: Size) extends Direct[A] with HasPreciseSize {
  def sizeInfo = size
  @inline final def foreach(f: A => Unit): Unit = size foreachIndex (i => f(elemAt(i)))
  def isDefinedAt(index: Index): Boolean = size containsIndex index
  override def toString = Foreach.stringify(this)(Show.native[A])
}

/** DirectAccess
 */

final class ArrayIsDirectAccess[A: ClassTag] extends DirectAccessImpl[A, Array[A], Direct] {
  def length(repr: Array[A]): Size            = Size(repr.length)
  def elemAt(repr: Array[A])(index: Index): A = repr(index.value)
}
final class IndexedSeqIsDirectAccess[CC[X] <: IndexedSeq[X], A] extends DirectAccessImpl[A, CC[A], CC] {
  def length(repr: CC[A]): Size            = Size(repr.length)
  def elemAt(repr: CC[A])(index: Index): A = repr(index)
}
final class PspIndexedIsDirectAccess[A0] extends DirectAccessImpl[A0, Direct[A0], Direct] {
  def length(repr: Direct[A0]): Size             = repr.size
  def elemAt(repr: Direct[A0])(index: Index): A0 = repr elemAt index
}

/** Foreachable
 */
final class TraversableIsForeachable[CC[X] <: Traversable[X], A] extends ForeachableImpl[A, CC[A], CC] {
  def foreach(repr: CC[A])(f: A => Unit): Unit = repr foreach f
}
final class PspForeachIsForeachable[A] extends ForeachableImpl[A, Foreach[A], Foreach] {
  def foreach(repr: Foreach[A])(f: A => Unit): Unit = repr foreach f
}
final class PspLinearIsSequentialAccess[A] extends SequentialAccessImpl[A, Linear[A], Linear] {
  def head(repr: Linear[A]): A          = repr.head
  def tail(repr: Linear[A]): Linear[A]  = repr.tail
  def isEmpty(repr: Linear[A]): Boolean = repr.isEmpty
}

trait DirectAccessImpl[A0, Repr, CC0[X]] extends DirectAccess[Repr] {
  @inline final def foreach(repr: Repr)(f: A => Unit): Unit = length(repr) foreachIndex (i => f(elemAt(repr)(i)))
  type CC[X] = CC0[X]
  type A     = A0
}

trait SequentialAccessImpl[A0, Repr, CC0[X]] extends SequentialAccess[Repr] {
  @inline def foreach(repr: Repr)(f: A => Unit): Unit = {
    @tailrec def loop(xs: Repr): Unit = if (!isEmpty(xs)) { f(head(xs)) ; loop(tail(xs)) }
    loop(repr)
  }
  type CC[X] = CC0[X]
  type A     = A0
}

trait ForeachableImpl[A0, Repr, CC0[X]] extends Foreachable[Repr] {
  type CC[X] = CC0[X]
  type A     = A0
}
