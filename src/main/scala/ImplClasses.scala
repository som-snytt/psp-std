package psp
package core
package impl

import scala.{ collection => sc }
import sc.{ mutable => scm }

/** Compat
 */

final class ScalaIndexedSeqAsIndexed[+A](underlying: sc.IndexedSeq[A]) extends IndexedImpl[A](Size(underlying.size)) {
  def elemAt(index: Index) = underlying(index)
  override def toString = underlying.shortClass + " (wrapped)"
}

final class TraversableAsForeach[+A](underlying: Traversable[A]) extends Foreach[A] {
  def sizeInfo: SizeInfo          = SizeInfo(underlying)
  def foreach(f: A => Unit): Unit = underlying foreach f
  override def toString           = underlying.shortClass + " (wrapped)"
}

final class ForeachAsTraversable[+A](underlying: Foreach[A]) extends sc.immutable.Traversable[A] {
  def foreach[U](f: A => U): Unit = underlying foreach (x => f(x))
}

/** ArrowAssoc
 */

final class ArrowAssocInt(private val self: Int) extends AnyVal {
  @inline def -> [@specialized(Int, Long, Double, Char, Boolean) B](y: B): Tuple2[Int, B] = Tuple2(self, y)
}
final class ArrowAssocLong(private val self: Long) extends AnyVal {
  @inline def -> [@specialized(Int, Long, Double, Char, Boolean) B](y: B): Tuple2[Long, B] = Tuple2(self, y)
}
final class ArrowAssocDouble(private val self: Double) extends AnyVal {
  @inline def -> [@specialized(Int, Long, Double, Char, Boolean) B](y: B): Tuple2[Double, B] = Tuple2(self, y)
}
final class ArrowAssocChar(private val self: Char) extends AnyVal {
  @inline def -> [@specialized(Int, Long, Double, Char, Boolean) B](y: B): Tuple2[Char, B] = Tuple2(self, y)
}
final class ArrowAssocBoolean(private val self: Boolean) extends AnyVal {
  @inline def -> [@specialized(Int, Long, Double, Char, Boolean) B](y: B): Tuple2[Boolean, B] = Tuple2(self, y)
}
final class ArrowAssocRef[A](private val self: A) extends AnyVal {
  @inline def -> [B](y: B): Tuple2[A, B] = Tuple2(self, y)
}

/** Indexed
 */

abstract class IndexedImpl[+A](val size: Size) extends Indexed[A] {
  def isDefinedAt(index: Index): Boolean = size containsIndex index
  @inline final def foreach(f: A => Unit): Unit = {
    var i = 0
    while (i < size.value) { f(elemAt(i)) ; i += 1 }
  }
  override def toString = Foreach stringify this
}

/** Indexable
 */
object StringIsIndexable extends IndexableImpl[Char, String, Indexed] {
  def length(repr: String): Size                     = Size(repr.length)
  def elemAt(repr: String)(index: Index): Char       = repr charAt index
}
final class ArrayIsIndexable[A: ClassTag] extends IndexableImpl[A, Array[A], Indexed] {
  def length(repr: Array[A]): Size            = Size(repr.length)
  def elemAt(repr: Array[A])(index: Index): A = repr(index)
}
final class IndexedSeqIsIndexable[CC[X] <: IndexedSeq[X], A] extends IndexableImpl[A, CC[A], CC] {
  def length(repr: CC[A]): Size            = Size(repr.length)
  def elemAt(repr: CC[A])(index: Index): A = repr(index)
}
final class PspIndexedIsIndexable[A0] extends IndexableImpl[A0, Indexed[A0], Indexed] {
  def length(repr: Indexed[A0]): Size             = repr.size
  def elemAt(repr: Indexed[A0])(index: Index): A0 = repr elemAt index
}

/** Foreachable
 */
final class TraversableIsForeachable[CC[X] <: Traversable[X], A] extends ForeachableImpl[A, CC[A], CC] {
  def foreach(repr: CC[A])(f: A => Unit): Unit = repr foreach f
}
final class PspForeachIsForeachable[A] extends ForeachableImpl[A, Foreach[A], Foreach] {
  def foreach(repr: Foreach[A])(f: A => Unit): Unit = repr foreach f
}
final class PspLinearIsLinearable[A] extends LinearableImpl[A, Linear[A], Linear] {
  def head(repr: Linear[A]): A          = repr.head
  def tail(repr: Linear[A]): Linear[A]  = repr.tail
  def isEmpty(repr: Linear[A]): Boolean = repr.isEmpty

  @inline def foreach(repr: Linear[A])(f: A => Unit): Unit = {
    @tailrec def loop(xs: Linear[A]): Unit = if (!xs.isEmpty) { f(xs.head) ; loop(xs.tail) }
    loop(repr)
  }
}

trait IndexableImpl[A0, Repr, CC0[X]] extends Indexable[Repr] {
  type CC[X] = CC0[X]
  type A     = A0
}

trait LinearableImpl[A0, Repr, CC0[X]] extends Linearable[Repr] {
  type CC[X] = CC0[X]
  type A     = A0
}

trait ForeachableImpl[A0, Repr, CC0[X]] extends Foreachable[Repr] {
  type CC[X] = CC0[X]
  type A     = A0
}
