package psp
package std
package lowlevel

import api._

final class CircularBuffer[A](capacity: Precise) extends Direct[A] with AndThis {
  assert(!capacity.isZero, capacity)

  private[this] def cap                 = capacity.sizeValue
  private[this] val buffer              = newArray[Any](capacity)
  private[this] var seen                = 0
  private[this] def writePointer        = seen % cap
  private[this] def readPointer         = if (isFull) writePointer else 0
  private[this] def setHead(x: A): Unit = buffer(writePointer) = x sideEffect (seen += 1)

  @inline def foreach(f: A => Unit): Unit = size.toIndexRange foreach (i => f(elemAt(i)))

  def head: A                        = elemAt(0.index)
  def isFull                         = seen >= cap
  def elemAt(index: Index): A        = buffer((readPointer + index.indexValue) % cap).castTo[A]
  def size: Precise                  = (capacity min seen.size).precisely
  def ++=(xs: Foreach[A]): this.type = andThis(xs foreach setHead)
  def += (x: A): this.type           = andThis(this setHead x)
  def push(x: A): A                  = if (isFull) head sideEffect setHead(x) else abort("push on non-full buffer")

  override def toString = s"CircularBuffer($size/$capacity)"
}

object CircularBuffer {
  def builder[A](capacity: Size): Builds[A, CircularBuffer[A]] = Builds(xs => CircularBuffer[A](capacity) ++= xs)
  def apply[A](capacity: Size): CircularBuffer[A]              = new CircularBuffer[A](capacity.precisely)
}
