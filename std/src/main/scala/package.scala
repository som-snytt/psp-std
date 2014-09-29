package psp

import java.nio.{ file => jnf }
import jnf.{ attribute => jnfa }

package object std extends psp.std.PackageLevel {
  type PathPredicate = Path => Boolean
  type DSFilter[A]   = java.nio.file.DirectoryStream.Filter[A]
  // type Paths         = Iterable[Path]
  type PathDirStream = DirectoryStream[Path]

  type CopyOption          = jnf.CopyOption
  type FileAttributeView   = jnfa.FileAttributeView
  type FileAttribute[A]    = jnfa.FileAttribute[A]
  type FileStore           = jnf.FileStore
  type FileSystem          = jnf.FileSystem
  type FileSystemProvider  = jnf.spi.FileSystemProvider
  type FileVisitOption     = jnf.FileVisitOption
  type FileVisitor[A]      = jnf.FileVisitor[A]
  type LinkOption          = jnf.LinkOption
  type OpenOption          = jnf.OpenOption
  type PosixFilePermission = jnfa.PosixFilePermission
  type SeekableByteChannel = java.nio.channels.SeekableByteChannel
  type UserPrincipal       = jnfa.UserPrincipal

  type JarFile             = java.util.jar.JarFile
  type jAttributeName      = java.util.jar.Attributes.Name
  type jFilePermissions    = jSet[PosixFilePermission]
  type jLineIterable       = jIterable[_ <: CharSequence]
  type jManifestJavaAttrs  = jMap[jAttributeName, String]
  type jManifestScalaAttrs = Map[jAttributeName, String]

  type PspList[A] = linear.List[A]
  val PspList     = linear.List

  implicit class ArrayViewOps[A](val repr: Array[A]) {
    def m: IndexedView[A, Array[A]] = new DirectAccess.ArrayIs[A] wrap repr
  }
  implicit class StringViewOps[A](val repr: String) {
    def m: IndexedView[Char, String] = DirectAccess.StringIs wrap repr
  }

  val NoPath: Path         = path("")
  val NoFile: File         = NoPath.toFile
  val NoUri: URI           = NoFile.toURI
  val NoFileTime: FileTime = java.nio.file.attribute.FileTime fromMillis Long.MinValue

  def fileSeparator                                         = java.io.File.separator
  def now(): FileTime                                       = java.nio.file.attribute.FileTime fromMillis System.currentTimeMillis
  def newFile(s: String): File                              = newPath(s).toFile
  def newUri(s: String): URI                                = java.net.URI create s
  def newPath(s: String): Path                              = jnf.Paths get s
  def newPath(xs: Seq[String]): Path                        = if (xs.isEmpty) NoPath else xs.tail.foldLeft(newPath(xs.head))(_ resolve _)
  def newTempDir(prefix: String, attrs: AnyFileAttr*): Path = jnf.Files.createTempDirectory(prefix, attrs: _*)

  // type Zero[A] = api.Zero[A]

  // def utf8 = java.nio.charset.Charset forName "UTF-8"
  // def isWhitespace(ch: Char): Boolean = java.lang.Character isWhitespace ch
  // def isISOControl(ch: Char): Boolean = java.lang.Character isISOControl ch

  // def text(s: String): Doc        = Text(s)
  // def show[A: DocShow](x: A): Doc = implicitly[DocShow[A]] show x

  // // implicit def kiamaToDoc(x: KP.Doc): Doc                                          = Kiama(x)
  // implicit def pspDocExtensions(x: Doc): PspDoc                                       = new PspDoc(x)
  // implicit def pspSeqDocExtensions(x: Seq[Doc]): PspSeqDoc                            = new PspSeqDoc(x)
  // implicit def pspSeqShowableExtensions[A: DocShow](x: Seq[A]): PspSeqShowable[A]     = new PspSeqShowable[A](x)
  // implicit def pspArrayShowableExtensions[A: DocShow](x: Array[A]): PspSeqShowable[A] = pspSeqShowableExtensions[A](x.toSeq)
  // // implicit def scalaAnyExtensions[A](x: A): ScalaAny[A]                            = new ScalaAny[A](x)
  // implicit def stringExtensions(x: String): JavaString                                = new JavaString(x)
  // implicit def hasShowToDoc[A: DocShow](x: A): Doc                                    = implicitly[DocShow[A]] show x
  // // implicit def genericWrapArray[T](xs: Array[T]): WrappedArray[T] = Predef.genericWrapArray[T](xs) // XXX

  // implicit class OldScalaAny[A](val x: A) { //  extends AnyVal {
  //   def doc(implicit shows: DocShow[A]): Doc = shows show x
  //   def noNull(implicit z: Zero[A]): A       = if (isNull) z.zero else x
  //   def isNull: Boolean                      = x.toRef eq null
  // }

  def NameTransformer = scala.reflect.NameTransformer
  type CSeq[+A]    = scala.collection.Seq[A]
  type CSet[A]     = scala.collection.Set[A]
  type CMap[K, +V] = scala.collection.Map[K, V]

  // type ISeq[+A]    = scala.collection.immutable.Seq[A]
  type ISet[A]     = scala.collection.immutable.Set[A]
  type IMap[K, +V] = scala.collection.immutable.Map[K, V]
  // type uV      = scala.annotation.unchecked.uncheckedVariance

  // nio
  type AnyFileAttr              = java.nio.file.attribute.FileAttribute[_]
  type BasicFileAttributes      = java.nio.file.attribute.BasicFileAttributes
  type DirectoryStream[A]       = java.nio.file.DirectoryStream[A]
  type DirectoryStreamFilter[A] = java.nio.file.DirectoryStream.Filter[A]
  type FileTime                 = java.nio.file.attribute.FileTime

  type ConcurrentHashMap[K, V] = java.util.concurrent.ConcurrentHashMap[K, V]
  // type sIterator[+A] = scala.collection.Iterator[A]
  // type sIterable[+A] = scala.collection.Iterable[A]
  type Bytes         = Array[Byte]
  type Chars         = Array[Char]

  // private def prop(name: String): String = System.getProperty(name, "")

  // final def scalaHome: Path        = newPath(prop("scala.home"))
  // final def javaHome: Path         = newPath(prop("java.home"))
  // final def userHome: Path         = newPath(prop("user.home"))
  // final def currentDirectory: Path = newPath(prop("user.dir"))

  // final def bootClassPath: Classpath          = Classpath(prop("sun.boot.class.path"))
  // final def javaClassPath: Classpath          = Classpath(prop("java.class.path"))
  // final def javaVersion: Version              = Version(prop("java.version"))
  // final def javaSpecificationVersion: Version = Version(prop("java.specification.version"))
  // final def javaClassVersion: Version         = Version(prop("java.class.version"))
  // final def osInfo: OsInfo                    = OsInfo(prop("os.arch"), prop("os.name"), prop("os.version"))

  // @inline final def doto[A](x: A)(f: A => Unit): A                      = { f(x) ; x }
  // @inline final def printResult[A](msg: String)(body: => A): A          = doto(body)(x => System.err.println(s"$msg: $x"))
  // @inline final def classTag[A](implicit tag: ClassTag[A]): ClassTag[A] = tag
  // @inline final def javaClassOf[A](implicit tag: ClassTag[A]): jClass   = tag.runtimeClass

  // def printResult[T](in: Any)(body: T): T = {
  //   System.err.println(s"$in: $body")
  //   body
  // }
  // @inline final def unbufferedPrintResult[T](in: Any)(body: => T): T = {
  //   System.err.print(s"$in: ")
  //   System.err.flush()
  //   val result = body
  //   System.err.println(result)
  //   result
  // }

  // def eol: Doc  = Doc.line

  // def classpathSeparator  = java.io.File.pathSeparator
  // def fileSeparator       = java.io.File.separator

  // def now(): FileTime                = attribute.FileTime fromMillis System.currentTimeMillis
  // def newFile(s: String): File       = newPath(s).toFile
  // def newUri(s: String): URI         = java.net.URI create s
  // def newPath(s: String): Path       = Paths get s
  // def newPath(xs: Seq[String]): Path = if (xs.isEmpty) NoPath else xs.tail.foldLeft(newPath(xs.head))(_ resolve _)

  // def newTempDir(prefix: String, attrs: AnyFileAttr*): Path = Files.createTempDirectory(prefix, attrs: _*)

  // def concurrentMap[K, V](): ConcurrentMapWrapper[K, V]           = new ConcurrentMapWrapper[K, V](new ConcurrentHashMap[K, V], None)
  // def concurrentMap[K, V](default: V): ConcurrentMapWrapper[K, V] = new ConcurrentMapWrapper[K, V](new ConcurrentHashMap[K, V], Some(default))
  // // def mutableMap[K, V](default: V): mutable.Map[K, V]             = mutable.Map[K, V]() withDefaultValue default
  // // def mutableSet[A](xs: A*): mutable.Set[A]                       = mutable.Set[A](xs: _*)
  // // def newBuffer[A](xs: A*): mutable.ArrayBuffer[A]                = mutable.ArrayBuffer[A](xs: _*)
  // // def vectorBuilder[A](): VectorBuilder[A]                        = Vector.newBuilder[A]
  // // def listBuilder[A](): ListBuffer[A]                             = mutable.ListBuffer[A]()
  // def literal(s: String): Doc                                     = text(s)

  // def fromUTF8(xs: Array[Byte]): Array[Char] = scala.io.Codec fromUTF8 xs

  // // def nullAs[A] : A = null.asInstanceOf[A]

  // private def bindDiskValue[A](path: Path) = new impl.OnDiskValue[A](path)

  // def bindValue[A](path: Path): api.Value[A] = {
  //   if (path.toFile.exists)
  //     bindDiskValue[A](path).cached
  //   else
  //     throw new RuntimeException(s"Path does not exist: $path")
  // }

  // def newValue[A](path: Path): api.CachingValue[A] =
  //   bindDiskValue[A](path.ensureFile).cached

  // def newValue[A](path: Path, initialValue: A): api.CachingValue[A] = {
  //   val v = bindDiskValue[A](path.zeroOut).cached
  //   v setNow initialValue
  //   v
  // }

  // import java.util.Date
  // import java.text.DateFormat._

  // // The formatting styles include FULL, LONG, MEDIUM, and SHORT.
  // // "int timeStyle", philistines
  // def timeString(timeStyle: Int = MEDIUM): Doc = (getTimeInstance(timeStyle) format new Date()).asis
  // def stackString(frames: Int = 50): Doc = ((new Throwable).getStackTrace drop 3 take frames).toSeq.joinLines

  // // @inline final implicit def anyExtensions[A](x: A): extension.ScalaAny[A]                             = new extension.ScalaAny[A](x)
  // @inline final implicit def anyRefExtensions[A <: AnyRef](x: A): extension.ScalaAnyRef[A]                = new extension.ScalaAnyRef[A](x)
  // // @inline final implicit def stringExtensions(x: String): extension.JavaString                            = new extension.JavaString(x)
  // @inline final implicit def pathExtensions(x: Path): extension.JavaPath                                  = new extension.JavaPath(x)
  // @inline final implicit def javaIterableExtensions[A](x: jIterable[A]): extension.JavaIterable[A]        = new extension.JavaIterable[A](x)
  // @inline final implicit def javaClassExtensions(x: jClass): extension.JavaClass                          = new extension.JavaClass(x)
  // @inline final implicit def fileTimeExtensions(x: FileTime): extension.JavaFileTime                      = new extension.JavaFileTime(x)
  // @inline final implicit def valueExtensions[A](x: api.Value[A]): extension.PspValue[A]               = new extension.PspValue[A](x)
  // // @inline final implicit def seqShownExtensions[A: DocShow](x: Seq[A]): extension.PspSeqShown[A]       = new extension.PspSeqShown[A](x)
  // // @inline final implicit def arrayShownExtensions[A: DocShow](x: Array[A]): extension.PspArrayShown[A] = new extension.PspArrayShown[A](x)

  // @inline final implicit def traversableExtensions[A, CC[X] <: Traversable[X]](x: CC[A]): extension.ScalaTraversable[A, CC] =
  //   new extension.ScalaTraversable[A, CC](x)

  // // implicit class SeqOfStrings(val elems: Seq[String]) extends SeqOfSomething[String]()(Show.string)

  // implicit def predicateToDirectoryFilter[A](p: A => Boolean): DirectoryStream.Filter[A] = new DirectoryStream.Filter[A] { def accept(entry: A) = p(entry) }
  // implicit def directoryStreamToIterable[A](stream: DirectoryStream[A]): Iterable[A]     = Bilingual.iterator(stream.iterator).toIterable //.asScala.toIterable
}
