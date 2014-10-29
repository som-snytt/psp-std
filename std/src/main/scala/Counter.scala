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
  private[this] val seen = jSet[Any]()
  def count: Int = seen.size
  def record[A](x: A): A = try x finally seen add x
  override def toString = s"$count"
}
