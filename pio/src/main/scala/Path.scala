package psp
package std
package pio

import api._
import java.nio.file.{ Files, FileSystems, FileVisitResult, StandardOpenOption, FileSystemException }
import StandardOpenOption._

final class PioPathOps[T <: Path](val path: T) extends AnyVal with JavaPathMethods

trait JavaPathMethods extends Any {
  def path: Path

  type Paths   = View[Path]
  type Strings = View[String]

  /** The recommended way to obtain these things.
   */
  def bytes(): Bytes   = PathBytes(path)
  def chars(): Chars   = PathChars(path)
  def lines(): Strings = PathLines(path)
  def slurp(): String  = PathSlurp(path)
  def toPioJar(): Jar  = PathJars(path)

  def foreach(f: Path => Unit): Unit = walk(PathVisitor(f))
  def map[A](f: Path => A): View[A]  = inView(foreach) map f
  def indices: IndexRange            = indexRange(0, path.getNameCount)
  def to_s: String                   = path.toString

  /** Related names and paths.
   */
  def / (segment: String): Path        = path resolve segment
  def absolute: Path                   = path.toAbsolutePath
  def children: Paths                  = entries()
  def extension: String                = if (filename containsChar '.') filename.dottedSegments.last.toLowerCase else ""
  def filename: String                 = pathname.toString
  def nearestDirectory: Path           = if (isDir) path else parent
  def normalize: Path                  = path.normalize
  def packageName: String              = nearestDirectory.to_s stripPrefix "/" replaceChar ('/' -> '.')
  def parent: Path                     = noNull(path.getParent(), path)
  def pathname: Path                   = path.getFileName
  def real(options: LinkOption*): Path = path.toRealPath(options: _*)
  def realPath: Path                   = path.toRealPath()
  def segments: Paths                  = 0 until path.getNameCount map path.getName
  def split(sep: Char): Paths          = path.toString splitChar sep map (x => std.path(x))

  /** I/O.
   */
  def appendLines(lines: Each[String]): Path                                      = writeNioLines(BiIterable(lines), CREATE, APPEND)
  def appendString(line: String): Path                                            = appendLines(Direct(line))
  def attrs[A <: BasicFileAttributes : CTag](opts: LinkOption*): A                = Files.readAttributes(path, classOf[A], opts: _*)
  def bufferedReader(implicit codec: Codec): BufferedReader                       = Files.newBufferedReader(path, codec.charSet)
  def bufferedWriter(options: OpenOption*)(implicit codec: Codec): BufferedWriter = Files.newBufferedWriter(path, codec.charSet, options: _*)
  def ensureDir(): Path                                                           = if (isDir) path else Files createDirectories path
  def ensureFile(): Path                                                          = if (isFile) path else Files createFile path
  def inputStream(options: OpenOption*): InputStream                              = Files.newInputStream(path, options: _*)
  def outputStream(options: OpenOption*): OutputStream                            = Files.newOutputStream(path, options: _*)
  def size: Long                                                                  = Files size path
  def writeBytes(bytes: Array[Byte]): Path                                        = Files.write(path, bytes)
  def writeLines(lines: Each[String]): Path                                       = writeNioLines(BiIterable(lines), CREATE)
  def writeString(line: String): Path                                             = writeLines(Direct(line))
  def zeroOut(): Path                                                             = path doto { p => Files deleteIfExists p ; Files createFile p }

  /** Booleans and other queries.
   */
  def containsJarOrZip: Boolean            = entries exists Jar.isJarOrZip
  def exists: Boolean                      = path.toFile.exists
  def hasExtension(exts: String*): Boolean = exts exists (_ equalsIgnoreCase extension)
  def isClassFile: Boolean                 = path hasExtension "class"
  def isDir: Boolean                       = path.isDirectory()
  def isFile: Boolean                      = path.isRegularFile()
  def isJarOrZip: Boolean                  = Jar isJarOrZip path
  def lastModified: FileTime               = attrs[BasicFileAttributes]().lastModifiedTime

  def deepJars: Paths = filterDeepFiles(_.isJarOrZip)

  /** Traversal.
   */
  def deepClasses: Paths                     = deepFiles filter (_.isClassFile)
  def deepDirs: Paths                        = deepEntries() filter (_.isDir)
  def deepEntries(): Paths                   = entries() |> (xs => xs ++ xs.flatMap(_.deepEntries))
  def deepFiles: Paths                       = deepEntries() filterNot (_.isDir)
  def deepPackageNames: Strings              = deepClasses.map(_.packageName).sortDistinct
  def entries(): Paths                       = containerEntries(_ => true)
  def files: Paths                           = filterChildren(pathFs, path, _.toFile.isFile)
  def filterEntries(p: PathPredicate): Paths = containerEntries(p)
  def subdirectories: Paths                  = filterChildren(pathFs, path, _.toFile.isDirectory)
  def lines(codec: Codec): Strings           = Files.readAllLines(path, codec.charSet).m

  def walk(f: (Path, BasicFileAttributes) => FileVisitResult): Unit = walk(PathVisitor(f))
  def walk(visitor: PathVisitor): Unit                              = Files.walkFileTree(path, visitor)

  def filterDeepFiles(p: PathPredicate): Paths = inView { f =>
    def loop(path: Path): Unit = if (path.isDir) path.entries() foreach loop else if (p(path)) f(path)
    loop(path)
  }

  def readNBytes(toRead: Int): Bytes = {
    val buf       = new Bytes(toRead)
    var numRead   = 0
    var remaining = toRead
    val in        = inputStream()

    def loop(): Bytes = {
      val n = in.read(buf, numRead, remaining)
      if (n <= 0) buf else {
        numRead += n
        remaining -= n
        loop()
      }
    }
    try loop() finally in.close()
  }

  private def fsOptions(kvs: (String, Any)*): jMap[String, _]                     = kvs.toMap.asJava
  private def getOrCreateFs(uri: jUri): FileSystem                                = Try(FileSystems getFileSystem uri) | FileSystems.newFileSystem(uri, fsOptions())
  private def pathFs: FileSystem                                                  = path.getFileSystem
  private def jarUri(append: String): jUri                                        = jUri(s"jar:file://$realPath$append")
  private def writeNioLines(lines: jIterable[String], options: OpenOption*): Path = Files.write(path, lines, utf8Charset, options: _*)

  private def filterChildren(fs: FileSystem, root: Path, p: Path => Boolean): Paths =
    fs.provider.newDirectoryStream(root, p) |> (s => try s.m.pvec finally s.close())

  private def containerEntries(p: PathPredicate): Paths = (
    if (path.isReadable && isDir)
      filterChildren(pathFs, path, p)
    else if (path.isFile && isJarOrZip)
      Try(getOrCreateFs(jarUri("")) |> (fs => filterChildren(fs, fs.provider getPath jarUri("!/"), p))) | exView()
    else
      exView()
  )
}
