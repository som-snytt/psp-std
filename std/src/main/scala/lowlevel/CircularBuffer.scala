package psp
package std
package lowlevel

import api._

final class CircularBuffer[A](capacity: Precise) extends Direct.DirectImpl[A] with AndThis {
  assert(!capacity.isZero, capacity)

  private[this] def cap                 = capacity.intSize
  private[this] val buffer              = newArray[Any](capacity)
  private[this] var seen                = 0L
  private[this] def writePointer: Int   = (seen % cap).safeToInt
  private[this] def readPointer         = if (isFull) writePointer else 0
  private[this] def setHead(x: A): Unit = buffer(writePointer) = x sideEffect (seen += 1)

  @inline def foreach(f: A => Unit): Unit = this foreachIndex (i => f(elemAt(i)))

  def head: A                        = elemAt(0.index)
  def isFull                         = seen >= cap
  def elemAt(index: Index): A        = buffer((readPointer + index.safeToInt) % cap).castTo[A]
  def size: Precise                  = capacity min Size(seen)
  def ++=(xs: Foreach[A]): this.type = andThis(xs foreach setHead)
  def += (x: A): this.type           = andThis(this setHead x)
  def push(x: A): A                  = if (isFull) head sideEffect setHead(x) else abort("push on non-full buffer")

  override def toString = s"CircularBuffer($size/$capacity)"
}

object CircularBuffer {
  def builder[A](capacity: Precise): Builds[A, CircularBuffer[A]] = Builds(xs => CircularBuffer[A](capacity) ++= xs)
  def apply[A](capacity: Precise): CircularBuffer[A]              = new CircularBuffer[A](capacity)
}
