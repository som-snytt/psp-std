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
trait PackageLevel extends Implicits with ImplicitRemoval with Creators with Aliases with ArrowAssocHigh with Utility {
  val EOL          = sys.props.getOrElse("line.separator", "\n")
  val NoIndex      = Index.undefined
  val NoNth        = Nth.undefined
  val ClassTag     = scala.reflect.ClassTag
  val NumericRange = scala.collection.immutable.NumericRange

  /** It's like "" + x, except, you know, for kids.
   */
  val `""` = ShowDirect("")

  def javaClassOf[T: ClassTag] : Class[T] = classTag[T].runtimeClass.castTo[Class[T]]
  def classTag[T: ClassTag] : ClassTag[T] = implicitly[ClassTag[T]]
  def encodeScala(s: String): String      = scala.reflect.NameTransformer encode s
  def decodeScala(s: String): String      = scala.reflect.NameTransformer decode s
}

/** Aliases for types I've had to import over and over and over again.
 *  Your list may vary.
 */
trait Aliases {
  // common scala
  type ArrayBuffer[A]                  = scala.collection.mutable.ArrayBuffer[A]
  type Builder[-Elem, +To]             = scala.collection.mutable.Builder[Elem, To]
  type CanBuildFrom[-From, -Elem, +To] = scala.collection.generic.CanBuildFrom[From, Elem, To] // ugh, it's so hideous
  type ClassTag[A]                     = scala.reflect.ClassTag[A]
  type Codec                           = scala.io.Codec
  type GenTraversableLike[+A, +Repr]   = scala.collection.GenTraversableLike[A, Repr]
  type GenTraversableOnce[+A]          = scala.collection.GenTraversableOnce[A]
  type IndexedSeq[+A]                  = scala.collection.immutable.IndexedSeq[A]
  type ListBuffer[A]                   = scala.collection.mutable.ListBuffer[A]
  type ScalaNumber                     = scala.math.ScalaNumber
  type TraversableLike[+A, CC[+X]]     = scala.collection.TraversableLike[A, CC[A]]
  type Try[+A]                         = scala.util.Try[A]
  type VectorBuilder[A]                = scala.collection.mutable.Builder[A, Vector[A]]
  type WrappedArray[A]                 = scala.collection.mutable.WrappedArray[A]

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
  // type FileTime            = java.nio.file.attribute.FileTime
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
  type jField                 = java.lang.reflect.Field
  type jFile                  = java.io.File
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
  type ?=>[-A, +B]            = PartialFunction[A, B]
  type CanBuildSelf[A, CC[X]] = CanBuildFrom[CC[A], A, CC[A]]
}

/** Various lame global-scope implicits, made to disappear with our friend null.
 *  This list is subject to renegotiation.
 */
trait ImplicitRemoval {
  val any2stringadd              = null
  val fallbackStringCanBuildFrom = null
  val tuple2ToZippedOps          = null
  val tuple3ToZippedOps          = null
  val unwrapString               = null

  val Boolean2boolean = null
  val Byte2byte       = null
  val Character2char  = null
  val Double2double   = null
  val Float2float     = null
  val Integer2int     = null
  val Long2long       = null
  val Short2short     = null

  val genericWrapArray = null
  val wrapBooleanArray = null
  val wrapByteArray    = null
  val wrapCharArray    = null
  val wrapDoubleArray  = null
  val wrapFloatArray   = null
  val wrapIntArray     = null
  val wrapLongArray    = null
  val wrapRefArray     = null
  val wrapShortArray   = null
  val wrapString       = null
  val wrapUnitArray    = null

  // We reimplement these.
  val augmentString   = null
  val unaugmentString = null

  val byteArrayOps, shortArrayOps, charArrayOps, intArrayOps, longArrayOps, floatArrayOps, doubleArrayOps = null
  val byteWrapper, shortWrapper, charWrapper, intWrapper, longWrapper, floatWrapper, doubleWrapper        = null
  val StringAdd, ArrowAssoc                                                                               = null
  // val genericArrayOps                                                                                     = null
}

trait Utility {
  def andTrue(x: Unit): Boolean  = true
  def andFalse(x: Unit): Boolean = false

  def nullAs[A] : A                             = (null: AnyRef).castTo[A]
  def decodeName(s: String): String             = scala.reflect.NameTransformer.decode(s)
  def log(msg: String): Unit                    = Console.err println msg
  def printResult[A](msg: String)(result: A): A = try result finally log(s"$msg: $result")

  def fromUTF8(xs: Array[Byte]): String = new String(scala.io.Codec fromUTF8 xs)

  def timed[A](body: => A): A = {
    val start = System.nanoTime
    try body finally log("Elapsed: %.3f ms" format (System.nanoTime - start) / 1e6)
  }
}

trait Creators {
  def nullStream(): InputStream           = new NullIputStream
  def nullLoader(): NullClassLoader       = new NullClassLoader
  def contextLoader: ClassLoader          = noNull(Thread.currentThread.getContextClassLoader, nullLoader)
  def loaderOf[A: ClassTag] : ClassLoader = noNull(javaClassOf[A].getClassLoader, nullLoader)

  def resourceString(name: String): String = fromUTF8(resource(name))
  def resource(name: String): Array[Byte] = (
    Try(contextLoader).fold(_ getResourceAsStream name slurp, _ =>
      Try(loaderOf[this.type]).fold(_ getResourceAsStream name slurp, _ => Array[Byte]())
    )
  )
  def readInto[A] : Read.ReadInto[A]               = Read.into[A]
  def order[A] : Order.By[A]                       = Order.by[A]
  def uri(x: String): URI                          = java.net.URI create x
  def url(x: String): URL                          = new URL(x)
  def index(x: Int): Index                         = Index(x)
  def offset(x: Int): Offset                       = Offset(x)
  def nth(x: Int): Nth                             = Nth(x)
  def indexRange(start: Int, end: Int): IndexRange = IndexRange.until(Index(start), Index(end))
  def Try[A](body: => A): Try[A]                   = scala.util.Try[A](body)
  def noNull[A](value: A, orElse: => A): A         = if (value == null) orElse else value

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
  def jHashSet[A] : jHashSet[A] = new jHashSet[A]
  def jList[A] : jArrayList[A]  = new jArrayList[A]

  // A few builders.
  def listBuilder[A](xs: A*)            = List.newBuilder[A] ++= xs
  def arrayBuilder[A: ClassTag](xs: A*) = Array.newBuilder[A] ++= xs
  def vectorBuilder[A](xs: A*)          = Vector.newBuilder[A] ++= xs
}

trait LowPriorityPspStd {
  // A weaker variation of Shown - use Show[A] if one can be found and toString otherwise.
  implicit def showableToTryShown[A](x: A)(implicit shows: Show[A] = Show.native[A]): TryShown = new TryShown(shows show x)
  // Deprioritize PartialOrder vs. Order since they both have comparison methods.
  implicit def hasPartialOrderExtensionOps[A: PartialOrder](x: A): PartialOrder.Ops[A] = new PartialOrder.Ops[A](x)
}

trait Implicits extends LowPriorityPspStd {
  // The typesafe non-toString-using show"..." interpolator.
  implicit def showStringContextOps(sc: StringContext): ShowInterpolator = new ShowInterpolator(sc)
  // Continuing the delicate dance against scala's hostile-to-correctness intrinsics.
  implicit def showableToShown[A: Show](x: A): Shown = new Shown(implicitly[Show[A]] show x)

  // We buried Predef's {un,}augmentString in favor of these.
  @inline final implicit def pspAugmentString(x: String): PspStringOps   = new PspStringOps(x)
  @inline final implicit def pspUnaugmentString(x: PspStringOps): String = x.toString

  // Extension methods for non-collection types.
  implicit def arrayExtensionOps[A](xs: Array[A]): ArrayExtensionOps[A]         = new ArrayExtensionOps[A](xs)
  implicit def anyExtensionOps[A](x: A): AnyExtensionOps[A]                     = new AnyExtensionOps[A](x)
  implicit def tryExtensionOps[A](x: scala.util.Try[A]): TryExtensionOps[A]     = new TryExtensionOps[A](x)
  implicit def intExtensionOps(x: Int): IntExtensionOps                         = new IntExtensionOps(x)
  implicit def inputStreamExtensionOps(x: InputStream): InputStreamExtensionOps = new InputStreamExtensionOps(x)
  implicit def longExtensionOps(x: Long): LongExtensionOps                      = new LongExtensionOps(x)

  // Extension methods which depend on a typeclass.
  // If the type class is attached at creation it can't be a value class.
  // So it has to be duplicated across every method which utilizes it.
  // Another victory against boilerplate.
  implicit def hasEqExtensionOps[A](x: A): Eq.Ops[A]           = new Eq.Ops[A](x)
  implicit def canReadExtensionOps(s: String): Read.CanReadOps = new Read.CanReadOps(s)
  // And already we're defeated in that regard - just too difficult to walk the line.
  implicit def hasOrderExtensionOps[A: Order](x: A): Order.Ops[A]   = new Order.Ops[A](x)
  implicit def orderExtensionOps[A](x: Order[A]): Order.OrderOps[A] = new Order.OrderOps[A](x)
  implicit def showExtensionOps[A](x: Show[A]): Show.ShowOps[A]     = new Show.ShowOps[A](x)

  // Extension methods for various collections. Mostly we try not to split hairs and attach to GenTraversableOnce.
  // It's not like you have any idea what the performance characteristics of the target are anyway.
  // If you think you do, and you ever call standard collections methods, then think again.
  implicit def sortedMapExtensionOps[K, V](xs: scala.collection.SortedMap[K, V]): SortedMapExtensionOps[K, V]                      = new SortedMapExtensionOps[K, V](xs)
  implicit def mapExtensionOps[K, V](xs: scala.collection.Map[K, V]): MapExtensionOps[K, V]                                        = new MapExtensionOps[K, V](xs)
  implicit def seqExtensionOps[CC[X] <: scala.collection.Seq[X], A](xs: CC[A]): SeqExtensionOps[CC, A]                             = new SeqExtensionOps[CC, A](xs)
  implicit def seqNthExtensionOps[CC[X] <: scala.collection.Seq[X], A](xs: CC[A]): AddNthApplyToSeq[CC, A]                         = new AddNthApplyToSeq[CC, A](xs)
  implicit def seqIndexExtensionOps[CC[X] <: scala.collection.Seq[X], A](xs: CC[A]): AddIndexApplyToSeq[CC, A]                     = new AddIndexApplyToSeq[CC, A](xs)
  implicit def genTraversableOnceExtensionOps[CC[X] <: GenTraversableOnce[X], A](xs: CC[A]): GenTraversableOnceExtensionOps[CC, A] = new GenTraversableOnceExtensionOps[CC, A](xs)
}
