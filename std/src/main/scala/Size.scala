package psp
package std

import api._
import Size.undefined

/** Value class can't wrap another value class. */
final class IntSize private[std] (val sizeValue: Int) extends AnyVal with Size {
  def + (n: Size): Size = if (isEmpty || n.isEmpty) undefined else Size(get + n.get)
  def - (n: Size): Size = if (isEmpty || n.isEmpty) undefined else Size(get - n.get)
  def * (n: Int): Size  = if (isEmpty) undefined else Size(get * n)
  def / (n: Int): Size  = if (isPositive) Size(get / n) else undefined
  def % (n: Int): Size  = if (isPositive) Size(get % n) else this

  def min(that: Size): Size = if (isEmpty || that.isEmpty) undefined else Size(get min that.get)
  def max(that: Size): Size = if (isEmpty || that.isEmpty) undefined else Size(get max that.get)
  def increment: Size       = if (isEmpty) this else Size(get + 1)
  def decrement: Size       = if (isEmpty) this else Size(get - 1)
  def precisely: Precise    = Precise(sizeValue)

  def get                      = if (isEmpty) abort("empty.get") else sizeValue
  def isEmpty                  = sizeValue < 0
  def isZero                   = sizeValue == 0
  def isPositive               = sizeValue > 0
  def toIntRange               = intRange(0, sizeValue)
  def toIndexRange: IndexRange = indexRange(0, sizeValue)
  def toOption: Option[Int]    = if (isEmpty) None else Some(sizeValue)

  @inline def mapIndices[A](f: Index => A): Direct[A] = toIndexRange map f
  @inline def foreachIndex(f: Index => Unit): Unit    = toIndexRange foreach f
  @inline def foreachNth(f: Nth => Unit): Unit        = toIndexRange foreach (i => f(i.toNth))

  def containsRange(range: IndexRange): Boolean = range.end.indexValue <= sizeValue
  def containsIndex(index: Index): Boolean      = toIndexRange contains index
  def lastIndex: Index                          = Index(get - 1) // effectively maps both undefined and zero to no index.
  def lastNth: Nth                              = Nth(get)

  override def toString = if (isEmpty) "undefined" else s"$get"
}

// Size is^Wshould be its own unapply (value class bugs drove us out for now)
object Size extends (Int => Size) {
  def undefined: Size               = new IntSize(-1)
  def apply(n: Int): Size           = new IntSize(n max 0)
  def unapply(s: Size): Option[Int] = s.toOption
  def impl(s: Size): IntSize        = new IntSize(s.get)
}
