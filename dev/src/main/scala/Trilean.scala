package psp
package std

import Trilean._

/** True, False, and Unknown. Used in defining partial orders.
 */
final class Trilean private (val value: Byte) extends AnyVal {
  def isFalse     = this == False
  def isTrue      = this == True
  def isUndefined = this == Undefined

  def unary_! : Trilean = this match {
    case True      => False
    case False     => True
    case Undefined => Undefined
  }
  def &&(that: Trilean): Trilean = (
    if (isFalse || that.isFalse) False
    else if (isTrue && that.isTrue) True
    else Undefined
  )
  def ||(that: Trilean): Trilean = (
    if (isTrue || that.isTrue) True
    else if (isFalse && that.isFalse) False
    else Undefined
  )

  def booleanValue: Boolean = this match {
    case True  => true
    case False => false
    case _     => abort("Not a Boolean value")
  }

  override def toString = this match {
    case True      => "true"
    case False     => "false"
    case Undefined => "<undef>"
  }
}

object Trilean {
  val Undefined = new Trilean(-1)
  val False     = new Trilean(0)
  val True      = new Trilean(1)
}
