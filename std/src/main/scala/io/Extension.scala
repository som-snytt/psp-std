package psp
package std
package io

object InputStreamExtensionOps {
  final val BufferSize = 8192
}
import InputStreamExtensionOps._

final class InputStreamExtensionOps(val in: InputStream) extends AnyVal {
  private def wrap[A] (f: InputStream => A): A = {
    val in = this.buffered()
    try f(in) finally in.close()
  }
  def slurp(): Array[Byte] = slurp(-1)
  def slurp(len: Int): Array[Byte] = {
    val buf = Array.newBuilder[Byte]
    if (len >= 0) buf sizeHint len
    wrap { in =>
      var offset = 0
      val arr = new Array[Byte](BufferSize)
      def loop() {
        if (offset < len || len < 0) {
          val read = in.read(arr, 0, BufferSize)
          if (read >= 0) {
            offset += read
            buf ++= (arr take read)
            loop()
          }
        }
      }
      loop()
      buf.result doto (xs => assert(len < 0 || xs.length == len, s"Could not read entire source ($offset of $len bytes)"))
    }
  }
  def buffered(): BufferedInputStream = in match {
    case buf: BufferedInputStream => buf
    case _                        => new BufferedInputStream(in)
  }
}

class NullIputStream extends InputStream {
  def read(): Int = -1
}
