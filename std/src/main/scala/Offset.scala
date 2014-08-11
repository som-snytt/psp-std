package psp
package std

/** Unlike an Index, an Offset can have any integer value.
 *  A negative offset is a positive offset from the other reference point.
 */
final class Offset private (val value: Int) extends AnyVal with Ordered[Offset] {
  def unary_- : Offset   = Offset(-value)
  def >>(n: Int): Offset = Offset(value + n)
  def <<(n: Int): Offset = Offset(value - n)

  def compare(that: Offset): Int = value compare that.value
  override def toString = s"$value"
}
object Offset extends (Int => Offset) {
  def apply(value: Int): Offset = new Offset(value)
}
