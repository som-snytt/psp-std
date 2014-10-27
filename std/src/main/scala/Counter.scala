package psp
package std

trait Counter[+A] {
  def peek: A
  def next(): A
}

class IntCounter(start: Int) extends Counter[Int] {
  private[this] val atomicInt = new AtomicInteger(start)

  def peek: Int   = atomicInt.get
  def next(): Int = atomicInt.getAndIncrement
  override def toString = s"IntCounter(@ $peek)"
}

object Counter {
  def apply(start: Int): IntCounter = new IntCounter(start)

  class Mapped[A, B](counter: Counter[A], f: A => B) extends Counter[B] {
    def peek: B   = f(counter.peek)
    def next(): B = f(counter.next())
    override def toString = s"Counter.Mapped(@ $peek)"
  }
}

class RecorderCounter() {
  private[this] var counted = 0
  def inc(): this.type = try this finally counted += 1
  def count: Int = counted
  def record[T](x: T): T = try x finally inc()
  override def toString = s"$count"
}

trait CountCalls {
  def counter: RecorderCounter
  def calls                  = counter.count
  def recordCall[T](x: T): T = counter record x
}
