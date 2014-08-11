package psp
package core

trait Labeled {
  def label: String
  final override def toString = label
}
