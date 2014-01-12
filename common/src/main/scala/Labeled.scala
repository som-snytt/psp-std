package psp
package common

trait Labeled {
  def label: String
  final override def toString = label
}
class LabeledFunction[-T, +R](f: T => R, val label: String) extends (T => R) with Labeled {
  def apply(x: T): R = f(x)
}
class LabeledPartialFunction[-T, +R](pf: PartialFunction[T, R], val label: String) extends PartialFunction[T, R] with Labeled {
  def isDefinedAt(x: T) = pf isDefinedAt x
  def apply(x: T): R = pf(x)
}

