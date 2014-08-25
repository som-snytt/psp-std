package psp
package std

import io._

/** Sorry there's no way to put mutable and immutable into the namespace.
 *  You can import them individually into every file until you die.
 */
import scala.collection.{ mutable, immutable }

/** Yes I know all about implicit classes.
 *  There's no way to write an implicit value class which doesn't hardcode
 *  its location into an object. Separating the implicit conversion from
 *  the class allows clients to build their own package object.
 *
 *  This is all a consequence of scala offering no means for managing namespaces,
 *  so namespace management has become hopelessly entangled with unrelated concerns
 *  like inheritance, specificity, method dispatch, and so forth.
 */
trait PackageLevel extends Implicits with ImplicitRemoval with Aliases with ArrowAssocHigh with PackageMethods {
  val EOL          = sys.props.getOrElse("line.separator", "\n")
  val NoIndex      = Index.undefined
  val NoNth        = Nth.undefined
  val ClassTag     = scala.reflect.ClassTag
  val NumericRange = scala.collection.immutable.NumericRange
  val ScalaNil     = scala.collection.immutable.Nil
  /** It's like "" + x, except, you know, for kids.
   */
  val `""` = Shown("")

  final val MaxInt  = Int.MaxValue
  final val MinInt  = Int.MinValue
  final val MaxLong = Long.MaxValue
  final val MinLong = Long.MinValue

  // With type aliases like these which include a type selection,
  // sometimes substitution fails and you get messages like
  // "found: Int, required: tc.A." It is bug, bug, bug city.
  // It can be worked a little bit by expanding the type
  // manually at the call sites where the bug hits (it's SI-8223).
  type AtomicView[Repr, W <: Walkable[Repr]]  = Env[Repr, W]#AtomicView
  type IndexedView[Repr, W <: Walkable[Repr]] = Env[Repr, W]#IndexedView
  type Env[Repr, W <: Walkable[Repr]]         = ViewEnvironment[W#A, Repr, W#CC]

  type ForeachableType[A0, Repr, CC0[X]] = Foreachable[Repr] {
    type A = A0
    type CC[B] = CC0[B]
  }
  type DirectAccessType[A0, Repr, CC0[X]] = DirectAccess[Repr] {
    type A = A0
    type CC[B] = CC0[B]
  }
}

/** Aliases for types I've had to import over and over and over again.
 *  Your list may vary.
 */
trait Aliases {
  // common scala
  type ArrayBuffer[A]                = scala.collection.mutable.ArrayBuffer[A]
  type Builder[-Elem, +To]           = scala.collection.mutable.Builder[Elem, To]
  type CanBuild[-Elem, +To]          = scala.collection.generic.CanBuildFrom[_, Elem, To]
  type ClassTag[A]                   = scala.reflect.ClassTag[A]
  type Codec                         = scala.io.Codec
  type GenTraversableLike[+A, +Repr] = scala.collection.GenTraversableLike[A, Repr]
  type GenTraversableOnce[+A]        = scala.collection.GenTraversableOnce[A]
  type IndexedSeq[+A]                = scala.collection.immutable.IndexedSeq[A]
  type LinearSeq[+A]                 = scala.collection.immutable.LinearSeq[A]
  type ListBuffer[A]                 = scala.collection.mutable.ListBuffer[A]
  type ScalaNumber                   = scala.math.ScalaNumber
  type TraversableLike[+A, CC[+X]]   = scala.collection.TraversableLike[A, CC[A]]
  type Try[+A]                       = scala.util.Try[A]
  type VectorBuilder[A]              = scala.collection.mutable.Builder[A, Vector[A]]
  type WrappedArray[A]               = scala.collection.mutable.WrappedArray[A]
  type GTOnce[+A]                    = scala.collection.GenTraversableOnce[A]

  // common annotations
  type switch  = scala.annotation.switch
  type tailrec = scala.annotation.tailrec
  type uV      = scala.annotation.unchecked.uncheckedVariance

  // java types which I acknowledge as victors in the battle for simple names.
  type BufferedInputStream    = java.io.BufferedInputStream
  type BufferedReader         = java.io.BufferedReader
  type BufferedWriter         = java.io.BufferedWriter
  type ByteArrayInputStream   = java.io.ByteArrayInputStream
  type ByteArrayOutputStream  = java.io.ByteArrayOutputStream
  type Charset                = java.nio.charset.Charset
  type DataInput              = java.io.DataInput
  type DataInputStream        = java.io.DataInputStream
  type DataOutputStream       = java.io.DataOutputStream
  type File                   = java.io.File
  type FileInputStream        = java.io.FileInputStream
  type FileOutputStream       = java.io.FileOutputStream
  type IOException            = java.io.IOException
  type InputStream            = java.io.InputStream
  type JarEntry               = java.util.jar.JarEntry
  type JarInputStream         = java.util.jar.JarInputStream
  type LinkedBlockingQueue[A] = java.util.concurrent.LinkedBlockingQueue[A]
  type ObjectInputStream      = java.io.ObjectInputStream
  type ObjectOutputStream     = java.io.ObjectOutputStream
  type OutputStream           = java.io.OutputStream
  type URI                    = java.net.URI
  type URL                    = java.net.URL
  type URLClassLoader         = java.net.URLClassLoader

  // java types for which the battle rages on.
  type jAbstractCollection[A] = java.util.AbstractCollection[A]
  type jArrayList[A]          = java.util.ArrayList[A]
  type jArray[A]              = Array[A with Object]
  type jClass                 = java.lang.Class[_]
  type jCollection[A]         = java.util.Collection[A]
  type jField                 = java.lang.reflect.Field
  type jFile                  = java.io.File
  type jHashMap[K, V]         = java.util.HashMap[K, V]
  type jHashSet[A]            = java.util.HashSet[A]
  type jIterable[+A]          = java.lang.Iterable[A @uV]
  type jIterator[+A]          = java.util.Iterator[A @uV]
  type jList[A]               = java.util.List[A]
  type jManifest              = java.util.jar.Manifest
  type jMap[K, V]             = java.util.Map[K, V]
  type jMethod                = java.lang.reflect.Method
  type jSet[A]                = java.util.Set[A]

  // scala types which I won't let win.
  type sIterator[+A] = scala.collection.Iterator[A]
  type sIterable[+A] = scala.collection.Iterable[A]

  // originals.
  type ?=>[-A, +B]   = PartialFunction[A, B]
  type Predicate[-A] = A => Boolean
  type Suspended[+A] = (A => Unit) => Unit
}

trait PackageMethods {
  def ?[A](implicit value: A): A                         = value // aka implicitly
  def Try[A](body: => A): Try[A]                         = scala.util.Try[A](body)
  def andFalse(x: Unit): Boolean                         = false
  def andTrue(x: Unit): Boolean                          = true
  def asExpected[A](body: Any): A                        = body.castTo[A]
  def classTag[T: ClassTag] : ClassTag[T]                = implicitly[ClassTag[T]]
  def contextLoader(): ClassLoader                       = noNull(Thread.currentThread.getContextClassLoader, nullLoader)
  def decodeName(s: String): String                      = scala.reflect.NameTransformer decode s
  def each[A](xs: GTOnce[A]): Foreach[A]                 = Foreach traversable xs
  def show[A: Show] : Show[A]                            = implicitly
  def eqBy[A]                                            = new EqBy[A]
  def fail(msg: String): Nothing                         = throw new RuntimeException(msg)
  def failEmpty(operation: String): Nothing              = throw new NoSuchElementException(s"$operation on empty collection")
  def fromUTF8(xs: Array[Byte]): String                  = new String(scala.io.Codec fromUTF8 xs)
  def index(x: Int): Index                               = Index(x)
  def indexRange(start: Int, end: Int): IndexRange       = IndexRange.until(Index(start), Index(end))
  def javaClassOf[T: ClassTag] : Class[T]                = classTag[T].runtimeClass.castTo[Class[T]]
  def labelpf[T, R](label: String)(pf: T ?=> R): T ?=> R = new LabeledPartialFunction(pf, label)
  def loaderOf[A: ClassTag] : ClassLoader                = noNull(javaClassOf[A].getClassLoader, nullLoader)
  def log(msg: String): Unit                             = Console.err println msg
  def nanoTime: Long                                     = System.nanoTime
  def noNull[A](value: A, orElse: => A): A               = if (value == null) orElse else value
  def nth(x: Int): Nth                                   = Nth(x)
  def nullAs[A] : A                                      = (null: AnyRef).castTo[A]
  def nullLoader(): NullClassLoader                      = new NullClassLoader
  def nullStream(): InputStream                          = new NullIputStream
  def offset(x: Int): Offset                             = Offset(x)
  def orderBy[A]                                         = new OrderBy[A]
  def printResult[A](msg: String)(result: A): A          = try result finally log(s"$msg: $result")
  def readInto[A] : Read.ReadInto[A]                     = Read.into[A]
  def resource(name: String): Array[Byte]                = Try(contextLoader) || loaderOf[this.type] fold (_ getResourceAsStream name slurp, _ => Array.empty)
  def resourceString(name: String): String               = fromUTF8(resource(name))
  def showBy[A]                                          = new ShowBy[A]
  def timed[A](body: => A): A                            = nanoTime |> (start => try body finally log("Elapsed: %.3f ms" format (nanoTime - start) / 1e6))
  def unknownSize                                        = SizeInfo.Unknown
  def uri(x: String): URI                                = java.net.URI create x
  def url(x: String): URL                                = uri(x).toURL

  // Mostly obviating the need for those mutable/immutable identifiers.
  def mutableSeq[A](xs: A*): mutable.Seq[A]                 = mutable.Seq(xs: _*)
  def mutableSet[A](xs: A*): mutable.Set[A]                 = mutable.Set(xs: _*)
  def mutableMap[K, V](kvs: (K, V)*): mutable.Map[K, V]     = mutable.Map[K, V](kvs: _*)
  def immutableSeq[A](xs: A*): immutable.Seq[A]             = immutable.Seq(xs: _*)
  def immutableSet[A](xs: A*): immutable.Set[A]             = immutable.Set(xs: _*)
  def immutableMap[K, V](kvs: (K, V)*): immutable.Map[K, V] = immutable.Map[K, V](kvs: _*)

  // OrderedMap is our own creation since SortedMap is way overspecified
  // and LinkedHashMap is too slow and only comes in a mutable variety.
  def orderedMap[K, V](kvs: (K, V)*): OrderedMap[K, V]                 = new OrderedMap[K, V](kvs map (_._1), kvs.toMap)
  def orderedMap[K, V](keys: Seq[K], map: Map[K, V]): OrderedMap[K, V] = new OrderedMap[K, V](keys, map)

  // Java.
  def jHashMap[K, V] : jHashMap[K, V] = new jHashMap[K, V]
  def jHashSet[A] : jHashSet[A]       = new jHashSet[A]
  def jList[A] : jArrayList[A]        = new jArrayList[A]

  // A few builders.
  def listBuilder[A](xs: A*)            = List.newBuilder[A] ++= xs
  def arrayBuilder[A: ClassTag](xs: A*) = Array.newBuilder[A] ++= xs
  def vectorBuilder[A](xs: A*)          = Vector.newBuilder[A] ++= xs
}
