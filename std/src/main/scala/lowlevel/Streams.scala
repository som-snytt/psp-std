package psp
package std
package lowlevel

import api._

object Streams {
  def slurp(in: BufferedInputStream): Array[Byte] = {
    val out = new ByteArrayOutputStream
    val buf = new Array[Byte](InputStreamBufferSize)
    def loop(): Array[Byte] = in read buf match {
      case -1 => out.toByteArray
      case n  => out.write(buf, 0, n) ; loop()
    }
    loop() sideEffect in.close()
  }
  def slurp(in: BufferedInputStream, size: Precise): Array[Byte] = {
    val len = size.safeInt
    val buf = newArray[Byte](len)
    def loop(remaining: Int): Array[Byte] = {
      if (remaining == 0) buf
      else in.read(buf, len - remaining, remaining) match {
        case -1 => buf
        case n  => loop(remaining - n)
      }
    }
    loop(len) sideEffect in.close()
  }
}
final class ByteBufferInputStream(b: ByteBuffer) extends InputStream {
  private def empty = !b.hasRemaining

  override def read()                                       = if (empty) -1 else b.get & 0xFF
  override def read(bytes: Array[Byte], off: Int, len: Int) = if (empty) -1 else len min2 b.remaining doto (b.get(bytes, off, _))
}

final class ByteBufferOutputStream(b: ByteBuffer) extends OutputStream {
  override def write(x: Int)                                 = b put x.toByte
  override def write(bytes: Array[Byte], off: Int, len: Int) = b.put(bytes, off, len)
}
