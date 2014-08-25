package psp
package std
package core

class Counter() {
  private[this] var counted = 0
  def inc(): this.type = try this finally counted += 1
  def count: Int = counted
  def record[T](x: T): T = try x finally inc()
}

trait CountCalls {
  def counter: Counter
  def calls                  = counter.count
  def recordCall[T](x: T): T = counter record x
}
