package psp
package std
package lowlevel

import api._
import ExclusiveIntRange._

final class ExclusiveIntRange private (val bits: Long) extends AnyVal with Direct.DirectImpl[Int] {
  def start: Int    = bits.left32
  def end: Int      = bits.right32
  def last: Int     = end - step
  def step: Int     = math.signum(end - start)
  def length: Int   = math.abs(end - start)
  def size: IntSize = Precise(length)
  def isDescending  = end < start
  def isAscending   = start < end

  private def nSteps(n: Precise): Int = (n * step).intSize

  def reverse: ExclusiveIntRange               = create(end - step, start - step)
  def drop(n: Precise): ExclusiveIntRange      = if (n.isZero) this else if (n >=  size) empty else create(start + nSteps(n), end)
  def dropRight(n: Precise): ExclusiveIntRange = if (n.isZero) this else if (n >=  size) empty else create(start, end - nSteps(n))
  def take(n: Precise): ExclusiveIntRange      = if (n.isZero) empty else if (n >= size) this else create(start, start + nSteps(n))
  def takeRight(n: Precise): ExclusiveIntRange = if (n.isZero) empty else if (n >= size) this else create(end - nSteps(n), end)
  def slice(s: Int, e: Int): ExclusiveIntRange = if (e <= 0 || e <= s) empty else this drop s take (e - s)
  def slice(r: IndexRange): ExclusiveIntRange  = slice(r.startInt, r.endInt)

  def suffixLength(p: Predicate[Int]): Int = reverse prefixLength p
  def prefixLength(p: Predicate[Int]): Int = {
    var elem = start
    var result = 0
    while (elem != end) {
      if (!p(elem)) return result
      result += 1
      elem += step
    }
    result
  }
  def dropWhile(p: Int => Boolean): ExclusiveIntRange = this drop prefixLength(p)
  def takeWhile(p: Int => Boolean): ExclusiveIntRange = {
    var cur = start
    while (cur < end && p(cur)) cur += 1
    start until cur
  }
  def >> (n: Int): ExclusiveIntRange = create(start + n, end + n)
  def << (n: Int): ExclusiveIntRange = create(start - n, end - n)

  def elemAt(i: Index): Int = start + i.safeInt
  def contains(x: Int): Boolean = if (isAscending) start <= x && x < end else start >= x && x > end

  @inline def foreach(f: Int => Unit): Unit        = if (!isEmpty) foreachInt(start, last, step, f)
  @inline def foreachReverse(f: Int => Unit): Unit = if (!isEmpty) foreachInt(last, start, -step, f)

  override def toString = if (start == end) "<empty>" else s"[$start..$end)"
}

object ExclusiveIntRange {
  val empty = new ExclusiveIntRange(0L)

  /** Can't refer directly to fields because scala bloats all the bytecode
   *  going through getters. This way the parameters are locals.
   */
  @inline def foreachInt(start: Int, last: Int, step: Int, f: Int => Unit): Unit = {
    var elem = start - step
    while (true) {
      elem += step
      f(elem)
      if (elem == last) return
    }
  }

  /** This apply squashes any end <= start to the empty range.
   *  Only the create method (or reverse) will create a descending range.
   */
  def apply(start: Int, end: Int): ExclusiveIntRange  = if (end <= start) empty else create(start, end)
  def create(start: Int, end: Int): ExclusiveIntRange = if (start == end) empty else create(start join64 end)
  def create(bits: Long): ExclusiveIntRange           = new ExclusiveIntRange(bits)
}
