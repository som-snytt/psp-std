package psp
package std

import api._

// TODO - Tremendous sameness to these classes and other potential ones, but
// abstracting over value classes without losing the value-class parts is
// nearly impossible from inside the language. Most likely we have to pursue
// code generation.

/** A valid index is always non-negative. All negative indices are
 *  mapped to NoIndex, which has an underlying value of -1.
 *  In principle we could double the usable range by treating
 *  value as an unsigned Int other than -1.
 *
 *  Manipulations of undefined values remain undefined, like NaN.
 */
final class IntIndex private[std] (val indexValue: Long) extends AnyVal with Index {
  def /(size: Int): Index           = this / Precise(size: Long)
  def %(size: Int): Index           = this % Precise(size: Long)
  def /(size: LongSize): Index      = if (isUndefined) this else Index(indexValue / size.value)
  def %(size: LongSize): Index      = if (isUndefined) this else Index(indexValue % size.value)
  def +(n: Long): Index             = if (isUndefined) this else Index(indexValue + n)
  def until(end: Index): IndexRange = indexRange(safeToInt, end.safeToInt)
  def toSize: Precise               = newSize(indexValue)
  def toBit1: Bit1                  = Bit1(this)
  def toIndex: Index                = this
  def toNth: Nth                    = Nth(indexValue + 1)
  def toOffset: Offset              = if (isUndefined) abort("undefined") else Offset(safeToInt)
  def safeToInt: Int                = indexValue.safeToInt
  def isUndefined                   = indexValue < 0
  override def toString             = if (isUndefined) "undefined" else s"$indexValue"
}

/** Nth is a 1-based index. The recorded indexValue is 0-based as with Index.
 */
final class IntNth private[std] (val nthValue: Long) extends AnyVal with Nth {
  def +(n: Long): Nth   = if (isEmpty) this else Nth(nthValue + n)
  def toIndex: Index    = Index(nthValue - 1)
  def toNth: Nth        = this
  def toOffset: Offset  = toIndex.toOffset
  def safeToInt: Int    = nthValue.safeToInt
  override def toString = if (isEmpty) "undefined" else s"#$nthValue"
}

/** Unlike an Index, an Offset can have any integer value.
 *  A negative offset is a positive offset from the other reference point.
 */
final class IntOffset private[std] (val offsetValue: Int) extends AnyVal with Offset {
  def get                            = offsetValue
  def until(end: Offset): IndexRange = indexRange(offsetValue, end.offsetValue)
  def +(n: Int): Offset              = Offset(offsetValue + n)
  def -(n: Int): Offset              = Offset(offsetValue - n)
  def unary_- : Offset               = Offset(-offsetValue)
  def toIndex: Index                 = Index(offsetValue)
  def toNth: Nth                     = toIndex.toNth
  def toOffset: Offset               = this
  private def sign                   = if (offsetValue < 0) "-" else if (offsetValue > 0) "+" else ""
  override def toString              = s"$sign$offsetValue"
}

object Index extends (Long => Index) {
  def undefined: Index             = new IntIndex(-1)
  def zero: Index                  = new IntIndex(0)
  def apply(value: Long): Index    = if (value < 0) undefined else new IntIndex(value)
  def unapply(x: IndexLike): Index = x.toIndex
  def impl(x: Index): IntIndex     = new IntIndex(x.get)
}
object Nth extends (Long => Nth) {
  def undefined: Nth             = new IntNth(-1)
  def apply(value: Long): Nth    = if (value <= 0) undefined else new IntNth(value)
  def unapply(x: IndexLike): Nth = x.toNth
  def impl(x: Nth): IntNth       = new IntNth(x.indexValue)
}
object Offset extends (Int => Offset) {
  def apply(value: Int): Offset  = new IntOffset(value)
  def unapply(x: Offset): Offset = x
  def impl(x: Offset): IntOffset = new IntOffset(x.offsetValue)
}
