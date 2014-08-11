package psp
package std

import ThreeValue._

/** True, False, and Unknown. Used in defining partial orders.
 */
final class ThreeValue private (val value: Byte) extends AnyVal {
  def isFalse     = this == False
  def isTrue      = this == True
  def isUndefined = this == Undefined

  def unary_! : ThreeValue = this match {
    case True      => False
    case False     => True
    case Undefined => Undefined
  }
  def &&(that: ThreeValue): ThreeValue = (
    if (isFalse || that.isFalse) False
    else if (isTrue && that.isTrue) True
    else Undefined
  )
  def ||(that: ThreeValue): ThreeValue = (
    if (isTrue || that.isTrue) True
    else if (isFalse && that.isFalse) False
    else Undefined
  )

  def booleanValue: Boolean = this match {
    case True  => true
    case False => false
    case _     => sys.error("Not a Boolean value")
  }

  override def toString = this match {
    case True      => "true"
    case False     => "false"
    case Undefined => "<undef>"
  }
}

object ThreeValue {
  val Undefined = new ThreeValue(-1)
  val False     = new ThreeValue(0)
  val True      = new ThreeValue(1)
}
