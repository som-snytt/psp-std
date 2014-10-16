package psp
package std
package pio

/** A byte array representing a classfile.
 */
sealed trait ClassBytes { def bytes: Bytes }
final case class OnlyBytes(bytes: Bytes) extends ClassBytes
final case class JarEntryAndBytes(entry: JarEntry, bytes: Bytes) extends ClassBytes
final case class PathAndBytes(path: Path, bytes: Bytes) extends ClassBytes

object ClassBytes {
  def apply(entry: JarEntry, bytes: Bytes): ClassBytes = JarEntryAndBytes(entry, bytes)
  def apply(path: Path, bytes: Bytes): ClassBytes      = PathAndBytes(path, bytes)
  def apply(bytes: Bytes): ClassBytes                  = OnlyBytes(bytes)

  implicit final class ClassBytesOps(val x: ClassBytes) extends AnyVal {
    def length: Int             = x.bytes.length
    def name: String            = x match {
      case JarEntryAndBytes(entry, _) => entry.getName
      case PathAndBytes(path, _)      => path.filename
      case OnlyBytes(xs)              => ""
    }
    def isClass = name endsWith ".class"
    def className: String = name stripSuffix ".class" replaceChar '/' -> '.'
    def string: String = x match {
      case OnlyBytes(_) => s"($length bytes)"
      case _            => s"$name ($length bytes)"
    }
  }
}

