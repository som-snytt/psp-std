package psp
package std
package pio

import api._

/** This interface is driven by the requirements for using JarFile and JarInputStream.
 *  without leaking.
 */
sealed trait Jar {
  def foreachEntry(f: JarEntry => Unit): Unit
  def foreachBytes(f: (JarEntry, Bytes) => Unit): Unit
  def foreachStream(f: (JarEntry, InputStream) => Unit): Unit
  def manifestMap: ManifestMap
}
object Jar {
  private def isZipHeader(bs: Bytes) = bs.length == 4 && bs(0) == 80 && bs(1) == 75 && bs(2) == 3 && bs(3) == 4
  private def hasZipHeader(f: Path)  = f.isRegularFile() && isZipHeader(f readNBytes 4)

  def isJarOrZip(f: Path): Boolean              = isJarOrZip(f, examineFile = false)
  def isJarOrZip(f: Path, examineFile: Boolean) = f.hasExtension("zip", "jar") || (examineFile && hasZipHeader(f))

  def apply(instream: => InputStream): Jar = new StreamJar(() => instream)
  def apply(url: jUrl): Jar                = new StreamJar(() => url.openStream())
  def apply(path: Path): Jar               = new FileJar(path)
}

final case class FileJar(path: Path) extends Jar  {
  def foreachEntry(f: JarEntry => Unit): Unit                 = onFile(jar => entryStream(jar) foreach f)
  def foreachStream(f: (JarEntry, InputStream) => Unit): Unit = onFile(jar => entryStream(jar) foreach (e => f(e, jar getInputStream e)))
  def foreachBytes(f: (JarEntry, Bytes) => Unit): Unit        = foreachStream((entry, in) => f(entry, in slurp entry.intSize))
  def manifestMap                                             = onFile(in => ManifestMap(in.getManifest))

  private def entryStream(jar: JarFile)     = jar.entries().toIterator filter (_.isClassFile)
  private def onFile[A](f: JarFile => A): A = new JarFile(path.toFile) |> (jar => try f(jar) finally jar.close())
}

final case class StreamJar(instream: () => InputStream) extends Jar {
  @inline def foreachEntry(f: JarEntry => Unit): Unit                 = onStream(in => entryStream(in) foreach f)
  @inline def foreachBytes(f: (JarEntry, Bytes) => Unit): Unit        = onStream(in => entryStream(in) foreach (e => applyToEntry(in, e)(f)))
  @inline def foreachStream(f: (JarEntry, InputStream) => Unit): Unit = foreachBytes(f comap2 (arr => new ByteArrayInputStream(arr)))
  def manifestMap                                                     = onStream(in => ManifestMap(in.getManifest))

  private def applyToEntry(in: JarInputStream, entry: JarEntry)(f: (JarEntry, Bytes) => Unit): Unit = {
    val arrSize = 256000
    val arr     = new Array[Byte](arrSize)
    def loop(offset: Int): Unit = in.read(arr, offset, arrSize - offset) match {
      case read if read >= 0 => loop(offset + read)
      case _                 => in.closeEntry() ; f(entry, arr take offset force)
    }
    loop(0)
  }
  private def entryStream(in: JarInputStream)        = Foreach.continuallySpan[JarEntry](_ ne null)(in.getNextJarEntry())
  private def onStream[A](f: JarInputStream => A): A = new JarInputStream(instream()) |> (in => try f(in) finally in.close())
}
final case object NoJar extends Jar {
  def foreachEntry(f: JarEntry => Unit): Unit                 = ()
  def foreachBytes(f: (JarEntry, Bytes) => Unit): Unit        = ()
  def foreachStream(f: (JarEntry, InputStream) => Unit): Unit = ()
  def manifestMap                                             = ManifestMap()
}
