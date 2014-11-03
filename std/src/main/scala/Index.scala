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
final class IndexImpl private[std] (val indexValue: Long) extends AnyVal with Index {
  def get: Long                     = indexValue
  def /(size: Int): Index           = this / Precise(size: Long)
  def %(size: Int): Index           = this % Precise(size: Long)
  def /(size: LongSize): Index      = if (isUndefined) this else Index(indexValue / size.value)
  def %(size: LongSize): Index      = if (isUndefined) this else Index(indexValue % size.value)
  def +(n: Long): Index             = if (isUndefined) this else Index(indexValue + n)
  def -(n: Long): Index             = if (isUndefined) this else Index(indexValue - n)
  def prev: Index                   = this - 1
  def next: Index                   = this + 1
  def until(end: Index): IndexRange = indexRange(safeInt, end.safeInt)
  def sizeExcluding: Precise        = newSize(indexValue)
  def sizeIncluding: Precise        = newSize(indexValue + 1)
  def toBit1: Bit1                  = Bit1(this)
  def toIndex: Index                = this
  def toNth: Nth                    = Nth(indexValue + 1)
  def toOffset: Offset              = if (isUndefined) abort("undefined") else Offset(safeInt)
  def safeInt: Int                  = indexValue.safeInt
  def isUndefined                   = indexValue < 0
  def isEmpty                       = isUndefined
  override def toString             = if (isUndefined) "undefined" else s"$indexValue"
}

/** Nth is a 1-based index. The recorded indexValue is 0-based as with Index.
 */
final class Nth private[std] (val nthValue: Long) extends AnyVal {
  def +(n: Long): Nth   = if (isUndefined) this else Nth(nthValue + n)
  def toIndex: Index    = Index(nthValue - 1)
  def toNth: Nth        = this
  def toOffset: Offset  = toIndex.toOffset
  def safeInt: Int      = nthValue.safeInt
  def isUndefined       = nthValue <= 0
  override def toString = if (isUndefined) "undefined" else s"#$nthValue"
}

/** Unlike an Index, an Offset can have any integer value.
 *  A negative offset is a positive offset from the other reference point.
 */
final class Offset private[std] (val offsetValue: Int) extends AnyVal {
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
  def undefined: Index              = new IndexImpl(-1)
  def zero: Index                   = new IndexImpl(0)
  def apply(value: Long): Index     = if (value < 0) undefined else new IndexImpl(value)
  def unapply(x: api.Index): Index  = x
  def impl(x: api.Index): IndexImpl = new IndexImpl(x.get)
}
object Nth extends (Long => Nth) {
  def undefined: Nth          = new Nth(-1)
  def apply(value: Long): Nth = if (value <= 0) undefined else new Nth(value)
  def unapply(x: Nth): Nth    = x
}
object Offset extends (Int => Offset) {
  def apply(value: Int): Offset  = new Offset(value)
  def unapply(x: Offset): Offset = x
}
