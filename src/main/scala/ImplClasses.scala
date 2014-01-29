package psp
package core
package impl


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

abstract class InvariantIndexedImpl[A](size: Size) extends IndexedImpl[A](size) with InvariantIndexed[A] {
}

/** Indexable
 */
object StringIsIndexable extends IndexableImpl[String, Indexed, Char] {
  def convert(repr: String): Indexed[Char]     = Indexed pure repr
  def length(repr: String): Size               = Size(repr.length)
  def elemAt(repr: String)(index: Index): Char = repr charAt index
}
final class ArrayIsIndexable[A] extends IndexableImpl[Array[A], Indexed, A] {
  def convert(repr: Array[A]): Indexed[A]     = Indexed pure repr
  def length(repr: Array[A]): Size            = Size(repr.length)
  def elemAt(repr: Array[A])(index: Index): A = repr(index)
}
final class IndexedSeqIsIndexable[CC[X] <: IndexedSeq[X], A] extends IndexableImpl[CC[A], CC, A] {
  def convert(repr: CC[A]): CC[A]          = repr
  def length(repr: CC[A]): Size            = Size(repr.length)
  def elemAt(repr: CC[A])(index: Index): A = repr(index)
}
final class PspIndexedIsIndexable[A] extends IndexableImpl[Indexed[A], Indexed, A] {
  def convert(repr: Indexed[A]): Indexed[A]     = repr
  def length(repr: Indexed[A]): Size            = repr.size
  def elemAt(repr: Indexed[A])(index: Index): A = repr elemAt index
}

trait IndexableImpl[-Repr, CC0[X], A0] extends Indexable[Repr] {
  type CC[X]     = CC0[X]
  type A         = A0
  type Input[+X] = Indexed[X]
}

/** Foreachable
 */
final class ScalaTraversableIsForeachable[CC[X] <: Traversable[X], A] extends ForeachableImpl[CC[A], CC, A] {
  def convert(repr: CC[A]): CC[A]              = repr
  def foreach(repr: CC[A])(f: A => Unit): Unit = repr foreach f
}
final class PspForeachIsForeachable[A] extends ForeachableImpl[Foreach[A], Foreach, A] {
  def convert(repr: Foreach[A]): Foreach[A]         = repr
  def foreach(repr: Foreach[A])(f: A => Unit): Unit = repr foreach f
}
final class PspViewableIsForeachable[A] extends ForeachableImpl[ElementalView[A], Foreach, A] {
  def convert(repr: ElementalView[A]): Foreach[A]         = repr.toForeach
  def foreach(repr: ElementalView[A])(f: A => Unit): Unit = repr.toForeach foreach f
}

trait ForeachableImpl[-Repr, CC0[X], A0] extends Foreachable[Repr] {
  type CC[X]     = CC0[X]
  type A         = A0
  type Input[+X] = Foreach[X]
}
