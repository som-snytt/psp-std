package psp
package std

trait IndexOrNth extends Any {
  type This <: IndexOrNth

  def +(n: Int): This
  def -(n: Int): This
  def next: This
  def prev: This

  def isUndefined: Boolean
  def value: Int
  def toInt: Int
  def toLong: Long
  def toNth: Nth
  def toIndex: Index

  // Concessions to reality
  def intIndex: Int
  def intNth: Int
}
