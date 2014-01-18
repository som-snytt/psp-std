package psp
package core

/** A thin abstraction over some questionable assumptions. */
trait PspTypes {
  type Index         = Int
  type Done          = Boolean
  type Suspended[+A] = (A => Unit) => Unit
  val MaxIndex       = Int.MaxValue
}
