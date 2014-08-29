package psp
package std
package api

/** Sorry there's no way to put mutable and immutable into the namespace.
 *  You can import them individually into every file until you die.
 */
import scala.collection.{ mutable, immutable }
import java.nio.file.Paths

trait PackageLevel extends PackageAliases with PackageMethods with PackageImplicits

object Ops {
  // Can't make the parameter private until 2.11.
  final class AnyOps[A](val __psp_x: A) extends AnyVal {
    private def x = __psp_x
    private implicit def liftAny[B](x: B): AnyOps[B] = new AnyOps[B](x)

    // "Maybe we can enforce good programming practice with annoyingly long method names."
    def castTo[U] : U = x.asInstanceOf[U]
    def toRef: AnyRef = castTo[AnyRef]

    // The famed forward pipe.
    @inline def |>[B](f: A => B): B   = f(x)
    @inline def doto(f: A => Unit): A = try x finally f(x)
    @inline def also(body: Unit): A   = x

    // Calling eq on Anys.
    def ref_==(y: Any): Boolean = x.toRef eq y.toRef
    def id_## : Int             = System identityHashCode x

    def maybe[B](pf: PartialFunction[A, B]): Option[B] = pf lift x
    def try_s[A1 >: A](implicit shows: Show[A1] = null): String = if (shows == null) any_s else shows show x
    def any_s: String = x match {
      case x: ShowDirect => x.to_s
      case _             => "" + x
    }
  }
}
trait Labeled {
  def label: String
  override def toString = label
}

/** The classic type class for turning string representations into typed values.
 */
trait Read[A] extends Any {
  def read(s: String): A
}

/** The classic type class for encoding string representations.
 */
trait Show[A] extends Any {
  def show(x: A): String
}

/** When a type class is more trouble than it's worth.
 *  Not overriding toString here to leave open the possibility of
 *  using a synthetic toString, e.g. of case classes.
 */
trait ShowDirect extends Any {
  def to_s: String
}

trait PackageImplicits extends Any {
  implicit def apiOpsAny[A](x: A): Ops.AnyOps[A] = new Ops.AnyOps[A](x)
}

/** Aliases for types I've had to import over and over and over again.
 *  Your list may vary.
 */
trait PackageAliases extends Any {
  // common scala
  type ArrayBuffer[A]                = scala.collection.mutable.ArrayBuffer[A]
  type Builder[-Elem, +To]           = scala.collection.mutable.Builder[Elem, To]
  type CanBuild[-Elem, +To]          = scala.collection.generic.CanBuildFrom[_, Elem, To]
  type ClassTag[A]                   = scala.reflect.ClassTag[A]
  type Codec                         = scala.io.Codec
  type GTOnce[+A]                    = scala.collection.GenTraversableOnce[A]
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
  type Path                   = java.nio.file.Path
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
  self: PackageAliases =>

  val EOL      = sys.props.getOrElse("line.separator", "\n")
  val ClassTag = scala.reflect.ClassTag

  final val MaxInt  = Int.MaxValue
  final val MinInt  = Int.MinValue
  final val MaxLong = Long.MaxValue
  final val MinLong = Long.MinValue

  def Try[A](body: => A): Try[A]                = scala.util.Try[A](body)
  def andFalse(x: Unit): Boolean                = false
  def andTrue(x: Unit): Boolean                 = true
  def asExpected[A](body: Any): A               = body.asInstanceOf[A]
  def classTag[T: ClassTag] : ClassTag[T]       = implicitly[ClassTag[T]]
  def fail(msg: String): Nothing                = throw new RuntimeException(msg)
  def file(s: String): jFile                    = new jFile(s)
  def fromUTF8(xs: Array[Byte]): String         = new String(scala.io.Codec fromUTF8 xs)
  def javaClassOf[T: ClassTag] : Class[T]       = classTag[T].runtimeClass.asInstanceOf[Class[T]]
  def nanoTime: Long                            = System.nanoTime
  def nullAs[A] : A                             = null.asInstanceOf[A]
  def path(s: String): Path                     = Paths get s
  def printResult[A](msg: String)(result: A): A = try result finally println(s"$msg: $result")
  def uri(x: String): URI                       = java.net.URI create x
  def url(x: String): URL                       = uri(x).toURL
  def dateTime(): String                        = new java.text.SimpleDateFormat("yyyyMMdd-HH-mm-ss") format new java.util.Date

  // Mostly obviating the need for those mutable/immutable identifiers.
  def mutableSeq[A](xs: A*): mutable.Seq[A]                 = mutable.Seq(xs: _*)
  def mutableSet[A](xs: A*): mutable.Set[A]                 = mutable.Set(xs: _*)
  def mutableMap[K, V](kvs: (K, V)*): mutable.Map[K, V]     = mutable.Map[K, V](kvs: _*)
  def immutableSeq[A](xs: A*): immutable.Seq[A]             = immutable.Seq(xs: _*)
  def immutableSet[A](xs: A*): immutable.Set[A]             = immutable.Set(xs: _*)
  def immutableMap[K, V](kvs: (K, V)*): immutable.Map[K, V] = immutable.Map[K, V](kvs: _*)

  // A few builders.
  def listBuilder[A](xs: A*)            = List.newBuilder[A] ++= xs
  def arrayBuilder[A: ClassTag](xs: A*) = Array.newBuilder[A] ++= xs
  def vectorBuilder[A](xs: A*)          = Vector.newBuilder[A] ++= xs
}
