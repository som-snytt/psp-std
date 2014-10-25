package psp
package std

import api._

final class BitVector(longs: Array[Long], val size: Precise) extends Direct.DirectImpl[Boolean] {
  private def nubSize  = (size % 64).getInt
  private def lastSize = if (isEmpty) 0 else if (nubSize > 0) nubSize else 64

  def apply64(index: Index): Bit64      = new Bit64(longs(index))
  def foreach(f: Boolean => Unit): Unit = size.indices foreach (i => f(elemAt(i)))
  def elemAt(index: Index): Boolean     = apply64(index / 64)(index % 64)

  override def toString = if (isEmpty) "" else {
    val xs: View[String] = longs dropRight 1 map (l => 64.size.padLeft(l.binary, '0'))
    val ys: String       = longs.last.binary take lastSize
    (xs :+ ys).mkString("\n")
  }
}

object BitVector {
  implicit def newBuilder: Builds[Boolean, BitVector] = Builds(xs => apply(xs.seq: _*))

  def compress(xs: Array[Boolean]): Array[Long] = (
    (Precise(xs.length) /+ 64).indices map { i =>
      val range = (i until i.next) * 64
      (xs slice range).foldl(0L)((res, b) => (res << 1) | b.toLong)
    } toArray
  )
  def apply(xs: Boolean*): BitVector = new BitVector(compress(xs.toArray), newSize(xs.size))
}


final class Bit64(val bits: Long) extends AnyVal {
  def apply(index: Index): Boolean = apply(Bit1(index))
  def apply(bit: Bit1): Boolean    = bits & bit.mask isNonZero
}
final class Bit1(val index: Int) extends AnyVal {
  def mask: Long = 1L << index
}

object Bit1 {
  def apply(index: Index): Bit1 = asserting(new Bit1(index.safeToInt))(64.size containsIndex index, s"Out of range: $index")
  def apply(index: Int): Bit1   = apply(Index(index))
}
