package psp
package std

trait Labeled {
  def label: String
  final override def toString = label
}
