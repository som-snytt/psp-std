package psp
package std
package api

/** Sorry there's no way to put package names such as mutable and immutable into
 *  the namespace. You can import them individually into every file until you die.
 */
import scala.collection.{ generic => scg, mutable => scm, immutable => sci }
import java.nio.{ file => jnf }
import jnf.{ attribute => jnfa }

/** Aliases for types I've had to import over and over and over again.
 *  Your list may vary.
 */
trait Aliases extends Any {
  // common scala
  type ArrayBuffer[A]                  = scm.ArrayBuffer[A]
  type Builder[-Elem, +To]             = scm.Builder[Elem, To]
  type CanBuildFrom[-From, -Elem, +To] = scg.CanBuildFrom[From, Elem, To]
  type CanBuild[-Elem, +To]            = scg.CanBuildFrom[_, Elem, To]
  type ClassTag[A]                     = scala.reflect.ClassTag[A]
  type Codec                           = scala.io.Codec
  type GTOnce[+A]                      = scala.collection.GenTraversableOnce[A]
  type IndexedSeq[+A]                  = sci.IndexedSeq[A]
  type LinearSeq[+A]                   = sci.LinearSeq[A]
  type ListBuffer[A]                   = scm.ListBuffer[A]
  type ScalaNumber                     = scala.math.ScalaNumber
  type TraversableLike[+A, CC[+X]]     = scala.collection.TraversableLike[A, CC[A]]
  type Try[+A]                         = scala.util.Try[A]
  type WrappedArray[A]                 = scm.WrappedArray[A]

  // common annotations
  type switch  = scala.annotation.switch
  type tailrec = scala.annotation.tailrec
  type uV      = scala.annotation.unchecked.uncheckedVariance

  // java types which I acknowledge as victors in the battle for simple names.
  type BufferedInputStream     = java.io.BufferedInputStream
  type BufferedReader          = java.io.BufferedReader
  type BufferedWriter          = java.io.BufferedWriter
  type ByteArrayInputStream    = java.io.ByteArrayInputStream
  type ByteArrayOutputStream   = java.io.ByteArrayOutputStream
  type Charset                 = java.nio.charset.Charset
  type ConcurrentHashMap[K, V] = java.util.concurrent.ConcurrentHashMap[K, V]
  type DataInput               = java.io.DataInput
  type DataInputStream         = java.io.DataInputStream
  type DataOutputStream        = java.io.DataOutputStream
  type FileInputStream         = java.io.FileInputStream
  type FileOutputStream        = java.io.FileOutputStream
  type IOException             = java.io.IOException
  type InputStream             = java.io.InputStream
  type JarEntry                = java.util.jar.JarEntry
  type JarFile                 = java.util.jar.JarFile
  type JarInputStream          = java.util.jar.JarInputStream
  type LinkedBlockingQueue[A]  = java.util.concurrent.LinkedBlockingQueue[A]
  type ObjectInputStream       = java.io.ObjectInputStream
  type ObjectOutputStream      = java.io.ObjectOutputStream
  type OutputStream            = java.io.OutputStream
  type SeekableByteChannel     = java.nio.channels.SeekableByteChannel
  type URLClassLoader          = java.net.URLClassLoader

  // java types for which the battle rages on.
  type jAbstractCollection[A] = java.util.AbstractCollection[A]
  type jArrayList[A]          = java.util.ArrayList[A]
  type jArray[A]              = Array[A with Object]
  type jAttributeName         = java.util.jar.Attributes.Name
  type jClass                 = java.lang.Class[_]
  type jCollection[A]         = java.util.Collection[A]
  type jDate                  = java.util.Date
  type jField                 = java.lang.reflect.Field
  type jFile                  = java.io.File
  type jFilePermissions       = jSet[PosixFilePermission]
  type jHashMap[K, V]         = java.util.HashMap[K, V]
  type jHashSet[A]            = java.util.HashSet[A]
  type jIterable[+A]          = java.lang.Iterable[A @uV]
  type jIterator[+A]          = java.util.Iterator[A @uV]
  type jLineIterable          = jIterable[_ <: CharSequence]
  type jList[A]               = java.util.List[A]
  type jManifest              = java.util.jar.Manifest
  type jManifestJavaAttrs     = jMap[jAttributeName, String]
  type jManifestScalaAttrs    = Map[jAttributeName, String]
  type jMap[K, V]             = java.util.Map[K, V]
  type jMethod                = java.lang.reflect.Method
  type jProperties            = java.util.Properties
  type jReader                = java.io.Reader
  type jSet[A]                = java.util.Set[A]
  type jUri                   = java.net.URI
  type jUrl                   = java.net.URL
  type jWriter                = java.io.Writer

  // java.nio.file classes
  type CopyOption               = jnf.CopyOption
  type DirectoryStreamFilter[A] = jnf.DirectoryStream.Filter[A]
  type DirectoryStream[A]       = jnf.DirectoryStream[A]
  type FileStore                = jnf.FileStore
  type FileSystem               = jnf.FileSystem
  type FileSystemProvider       = jnf.spi.FileSystemProvider
  type FileVisitOption          = jnf.FileVisitOption
  type FileVisitor[A]           = jnf.FileVisitor[A]
  type LinkOption               = jnf.LinkOption
  type OpenOption               = jnf.OpenOption
  type Path                     = jnf.Path

  type AnyFileAttr              = jnfa.FileAttribute[_]
  type BasicFileAttributes      = jnfa.BasicFileAttributes
  type FileAttributeView        = jnfa.FileAttributeView
  type FileAttribute[A]         = jnfa.FileAttribute[A]
  type FileTime                 = jnfa.FileTime
  type PosixFilePermission      = jnfa.PosixFilePermission
  type UserPrincipal            = jnfa.UserPrincipal

  // scala types which I won't let win.
  type scIterator[+A] = scala.collection.Iterator[A]
  type scIterable[+A] = scala.collection.Iterable[A]
  type scMap[K, +V]   = scala.collection.Map[K, V]
  type scSeq[+A]      = scala.collection.Seq[A]
  type scSet[A]       = scala.collection.Set[A]
  type sciMap[K, +V]  = sci.Map[K, V]
  type sciSeq[+A]     = sci.Seq[A]
  type sciSet[A]      = sci.Set[A]
  type sciBitSet      = sci.BitSet
  type scmMap[K, V]   = scm.Map[K, V]
  type scmSeq[A]      = scm.Seq[A]
  type scmSet[A]      = scm.Set[A]

  // originals.
  type ?=>[-A, +B]   = PartialFunction[A, B]
  type Bytes         = Array[Byte]
  type CTag[A]       = scala.reflect.ClassTag[A]
  type Chars         = Array[Char]
  type PathDirStream = DirectoryStream[Path]
  type PathPredicate = Path => Boolean
  type Predicate[-A] = A => Boolean
  type Suspended[+A] = (A => Unit) => Unit
}
