package psp
package std

trait IndexOrNth extends Any {
  type This <: IndexOrNth

  // Name based extractors
  def get: Int
  def isEmpty: Boolean

  def +(n: Int): This
  def -(n: Int): This
  def next: This
  def prev: This

  def isDefined: Boolean
  def value: Int
  def toInt: Int
  def toLong: Long
  def toNth: Nth
  def toIndex: Index

  // Concessions to reality
  def intIndex: Int
  def intNth: Int
}
