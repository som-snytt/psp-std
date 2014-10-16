package psp
package dmz

import scala._
import scala.annotation.unchecked.{ uncheckedVariance => uV }
import java.nio.{ file => jnf }
import jnf.{ attribute => jnfa }
import java.lang.String

trait JavaLibrary extends Any {
  // Exceptional factories.
  def assertionError(msg: String): Nothing                = throw new AssertionError(msg)
  def ioException(msg: String): Nothing                   = throw new IOException(msg)
  def illegalArgumentException(msg: String): Nothing      = throw new IllegalArgumentException(msg)
  def noSuchElementException(msg: String): Nothing        = throw new NoSuchElementException(msg)
  def unsupportedOperationException(msg: String): Nothing = throw new UnsupportedOperationException(msg)
  def runtimeException(msg: String): Nothing              = throw new RuntimeException(msg)

  // A selection of popular static methods from javaland.
  def classpathSeparator: String     = java.io.File.pathSeparator
  def currentThread: Thread          = java.lang.Thread.currentThread
  def defaultCharset: Charset        = java.nio.charset.Charset.defaultCharset
  def fileSeparator: String          = java.io.File.separator
  def identityHashCode[A](x: A): Int = java.lang.System.identityHashCode(x)
  def milliTime: Long                = java.lang.System.currentTimeMillis
  def nanoTime: Long                 = java.lang.System.nanoTime
  def systemClassLoader: ClassLoader = java.lang.ClassLoader.getSystemClassLoader
  def utf8Charset: Charset           = java.nio.charset.Charset forName "UTF-8"

  // Exceptions and Throwables.
  type AssertionError                = java.lang.AssertionError
  type Exception                     = java.lang.Exception
  type IOException                   = java.io.IOException
  type IllegalArgumentException      = java.lang.IllegalArgumentException
  type LinkageError                  = java.lang.LinkageError
  type NoClassDefFoundError          = java.lang.NoClassDefFoundError
  type NoSuchElementException        = java.util.NoSuchElementException
  type RuntimeException              = java.lang.RuntimeException
  type Throwable                     = java.lang.Throwable
  type UnsupportedOperationException = java.lang.UnsupportedOperationException

  // java types which I acknowledge as victors in the battle for simple names.
  type AtomicInteger           = java.util.concurrent.atomic.AtomicInteger
  type AtomicLong              = java.util.concurrent.atomic.AtomicLong
  type BufferedInputStream     = java.io.BufferedInputStream
  type BufferedReader          = java.io.BufferedReader
  type BufferedWriter          = java.io.BufferedWriter
  type ByteArrayInputStream    = java.io.ByteArrayInputStream
  type ByteArrayOutputStream   = java.io.ByteArrayOutputStream
  type CharSequence            = java.lang.CharSequence
  type Charset                 = java.nio.charset.Charset
  type ClassLoader             = java.lang.ClassLoader
  type Class[A]                = java.lang.Class[A]
  type Comparable[A]           = java.lang.Comparable[A]
  type Comparator[-A]          = java.util.Comparator[A @uV]
  type ConcurrentHashMap[K, V] = java.util.concurrent.ConcurrentHashMap[K, V]
  type DataInput               = java.io.DataInput
  type DataInputStream         = java.io.DataInputStream
  type DataOutputStream        = java.io.DataOutputStream
  type FileInputStream         = java.io.FileInputStream
  type FileOutputStream        = java.io.FileOutputStream
  type InputStream             = java.io.InputStream
  type JarEntry                = java.util.jar.JarEntry
  type JarFile                 = java.util.jar.JarFile
  type JarInputStream          = java.util.jar.JarInputStream
  type LinkedBlockingQueue[A]  = java.util.concurrent.LinkedBlockingQueue[A]
  type Object                  = java.lang.Object
  type ObjectInputStream       = java.io.ObjectInputStream
  type ObjectOutputStream      = java.io.ObjectOutputStream
  type OutputStream            = java.io.OutputStream
  type PrintStream             = java.io.PrintStream
  type SeekableByteChannel     = java.nio.channels.SeekableByteChannel
  type StackTraceElement       = java.lang.StackTraceElement
  type Thread                  = java.lang.Thread
  type URLClassLoader          = java.net.URLClassLoader
  type Void                    = java.lang.Void

  // java types for which the battle rages on.
  type jAbstractCollection[A]                  = java.util.AbstractCollection[A]
  type jAnnotation                             = java.lang.annotation.Annotation
  type jArrayList[A]                           = java.util.ArrayList[A]
  type jArray[A]                               = Array[A with Object]
  type jAttributeName                          = java.util.jar.Attributes.Name
  type jClass                                  = java.lang.Class[_]
  type jClassLoader                            = java.lang.ClassLoader
  type jCollection[A]                          = java.util.Collection[A]
  type jDate                                   = java.util.Date
  type jEnumeration[A]                         = java.util.Enumeration[A]
  type jField                                  = java.lang.reflect.Field
  type jFile                                   = java.io.File
  type jFilePermissions                        = jSet[PosixFilePermission]
  type jFileTime                               = jnfa.FileTime
  type jGenericArrayType                       = java.lang.reflect.GenericArrayType
  type jGenericDeclaration                     = java.lang.reflect.GenericDeclaration
  type jHashMap[K, V]                          = java.util.HashMap[K, V]
  type jHashSet[A]                             = java.util.HashSet[A]
  type jIterable[+A]                           = java.lang.Iterable[A @uV]
  type jIterator[+A]                           = java.util.Iterator[A @uV]
  type jLineIterable                           = jIterable[_ <: CharSequence]
  type jList[A]                                = java.util.List[A]
  type jLocale                                 = java.util.Locale
  type jManifest                               = java.util.jar.Manifest
  type jMap[K, V]                              = java.util.Map[K, V]
  type jMethod                                 = java.lang.reflect.Method
  type jPackage                                = java.lang.Package
  type jParameterizedType                      = java.lang.reflect.ParameterizedType
  type jProperties                             = java.util.Properties
  type jReader                                 = java.io.Reader
  type jSet[A]                                 = java.util.Set[A]
  type jType                                   = java.lang.reflect.Type
  type jTypeVariable[D <: jGenericDeclaration] = java.lang.reflect.TypeVariable[D]
  type jTypeVar                                = jTypeVariable[_] // <: jGenericDeclaration]
  type jUri                                    = java.net.URI
  type jUrl                                    = java.net.URL
  type jWildcardType                           = java.lang.reflect.WildcardType
  type jWriter                                 = java.io.Writer

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

  // NIO attribute types
  type AnyFileAttr         = jnfa.FileAttribute[_]
  type BasicFileAttributes = jnfa.BasicFileAttributes
  type FileAttributeView   = jnfa.FileAttributeView
  type FileAttribute[A]    = jnfa.FileAttribute[A]
  type FileTime            = jnfa.FileTime
  type PosixFilePermission = jnfa.PosixFilePermission
  type UserPrincipal       = jnfa.UserPrincipal
}
