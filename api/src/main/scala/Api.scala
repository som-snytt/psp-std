package psp
package std
package api

/** Sorry there's no way to put mutable and immutable into the namespace.
 *  You can import them individually into every file until you die.
 */
import scala.collection.{ mutable, immutable }
import java.nio.file.Paths
import scala.sys.process.Process
import scala.sys.process.ProcessBuilder

trait PackageLevelPlusCompat extends PackageLevel with ScalaCompat

/** Leaves out scala compat and IO.
 */
trait PackageLevel extends PackageValues with PackageAliases with PackageMethods with PackageImplicits

/** When a type class is more trouble than it's worth.
 *  Not overriding toString here to leave open the possibility of
 *  using a synthetic toString, e.g. of case classes.
 *  ShowDirectNow performs that override.
 */
trait ShowDirect extends Any { def to_s: String }
trait ShowDirectNow extends Any with ShowDirect { final override def toString = to_s }

/** Used to achieve type-safety in the show interpolator.
 *  It's the String resulting from passing a value through its Show instance.
 */
final case class Shown(to_s: String) extends AnyVal with ShowDirectNow
final case class TryShown(to_s: String) extends AnyVal with ShowDirectNow

/** An incomplete selection of show compositors.
 *  Not printing the way scala does.
 *  Not included in api.PackageLevel, but available in psp-api.
 *  Included in std.PackageLevel.
 */
trait ShowImplicits {
  def inBrackets[A](xs: A*)(implicit shows: Show[A]): String = xs map shows.show mkString ("[", ", ", "]")

  implicit def booleanShow: Show[Boolean]               = Show.native()
  implicit def charShow: Show[Char]                     = Show.native()
  implicit def doubleShow: Show[Double]                 = Show.native()
  implicit def intShow: Show[Int]                       = Show.native()
  implicit def longShow: Show[Long]                     = Show.native()
  implicit def numberShow: Show[scala.math.ScalaNumber] = Show.native()
  implicit def showDirect: Show[ShowDirect]             = Show(_.to_s)
  implicit def stringShow: Show[String]                 = Show(x => x)

  implicit def indexShow: Show[api.Index]               = new Ops.ShowBy[api.Index] apply (_.value.toString)
  implicit def sizeShow: Show[api.Size]                 = new Ops.ShowBy[api.Size] apply (_.value.toString)

  implicit def arrayShow[A: Show] : Show[Array[A]]        = Show(xs => inBrackets(xs: _*))
  implicit def optShow[A: Show] : Show[Option[A]]         = Show(_.fold("-")(implicitly[Show[A]].show))
  implicit def seqShow[A: Show] : Show[Seq[A]]            = Show(xs => inBrackets(xs: _*))
  implicit def tupleShow[A: Show, B: Show] : Show[(A, B)] = Show { case (x, y) => "%s -> %s".format(implicitly[Show[A]] show x, implicitly[Show[B]] show y) }

  implicit def sizeInfoShow: Show[SizeInfo] = Show[SizeInfo] {
    case Bounded(lo, Infinite) => "[%s, <inf>)".format(lo.toString)
    case Bounded(lo, hi)       => "[%s, %s]".format(lo.toString, hi.toString)
    case Precise(size)         => size.toString
    case Infinite              => "<inf>"
  }
}

trait PackageImplicits0 extends Any {
  // A weaker variation of Shown - use Show[A] if one can be found and toString otherwise.
  implicit def showableToTryShown[A](x: A)(implicit shows: TryShow[A]): TryShown = new TryShown(shows show x)
}
trait PackageImplicits extends Any with PackageImplicits0 {
  implicit def opsApiAny[A](x: A): Ops.ApiAnyOps[A]           = new Ops.ApiAnyOps[A](x)
  implicit def opsOption[A](x: Option[A]): Ops.OptionOps[A]   = new Ops.OptionOps[A](x)
  implicit def opsTry[A](x: scala.util.Try[A]): Ops.TryOps[A] = new Ops.TryOps[A](x)

  // The typesafe non-toString-using show"..." interpolator.
  implicit def opsApiShowInterpolator(sc: StringContext): Ops.ShowInterpolator = new Ops.ShowInterpolator(sc)

  // Continuing the delicate dance against scala's hostile-to-correctness intrinsics.
  implicit def showableToShown[A: Show](x: A): Shown = Shown(implicitly[Show[A]] show x)

  // Ops on base type classes.
  implicit def opsApiOrder[A](x: Order[A]): Ops.OrderOps[A] = new Ops.OrderOps[A](x)
}

/** Aliases for types I've had to import over and over and over again.
 *  Your list may vary.
 */
trait PackageAliases extends Any {
  // common scala
  type ArrayBuffer[A]                  = scala.collection.mutable.ArrayBuffer[A]
  type Builder[-Elem, +To]             = scala.collection.mutable.Builder[Elem, To]
  type CanBuild[-Elem, +To]            = scala.collection.generic.CanBuildFrom[_, Elem, To]
  type CanBuildFrom[-From, -Elem, +To] = scala.collection.generic.CanBuildFrom[From, Elem, To]
  type ClassTag[A]                     = scala.reflect.ClassTag[A]
  type Codec                           = scala.io.Codec
  type GTOnce[+A]                      = scala.collection.GenTraversableOnce[A]
  type IndexedSeq[+A]                  = scala.collection.immutable.IndexedSeq[A]
  type LinearSeq[+A]                   = scala.collection.immutable.LinearSeq[A]
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
  type jProperties            = java.util.Properties
  type jReader                = java.io.Reader
  type jSet[A]                = java.util.Set[A]
  type jWriter                = java.io.Writer

  // scala types which I won't let win.
  type sIterator[+A] = scala.collection.Iterator[A]
  type sIterable[+A] = scala.collection.Iterable[A]

  // originals.
  type ?=>[-A, +B]   = PartialFunction[A, B]
  type Predicate[-A] = A => Boolean
  type Suspended[+A] = (A => Unit) => Unit
  type ISeq[+A]      = scala.collection.immutable.Seq[A]
}

trait PackageValues {
  val EOL      = sys.props.getOrElse("line.separator", "\n")
  val ClassTag = scala.reflect.ClassTag

  final val MaxInt  = Int.MaxValue
  final val MinInt  = Int.MinValue
  final val MaxLong = Long.MaxValue
  final val MinLong = Long.MinValue
}

/** These conflict with sbt and defeat our ambition of being
 *  able to import sbt._ and psp.libsbt._ without a bunch of
 *  pointless conflicts. Thanks scala.
 */
trait JavaIO {
  type File = java.io.File
  type Path = java.nio.file.Path
  type URI  = java.net.URI
  type URL  = java.net.URL

  def file(s: String): File = new File(s)
  def path(s: String): Path = Paths get s
  def uri(x: String): URI   = java.net.URI create x
  def url(x: String): URL   = uri(x).toURL

  def javaHome: File                           = file(scala.util.Properties.javaHome)
  def classpathSeparator: String               = java.io.File.pathSeparator
  def openInApp(app: String, file: File): Unit = execute("open", "-a", app, file.getAbsolutePath)
  def openSafari(file: File): Unit             = openInApp("Safari", file)
  def openChrome(file: File): Unit             = openInApp("Google Chrome", file)

  def newProcess(line: String): ProcessBuilder      = Process(line)
  def newProcess(args: Seq[String]): ProcessBuilder = Process(args)
  def executeLine(line: String): Int                = Process(line).!
  def execute(args: String*): Int                   = Process(args.toSeq).!
}

trait PackageMethods extends Any {
  import scala.reflect.ClassTag

  def eqBy[A]    = new Ops.EqBy[A]
  def orderBy[A] = new Ops.OrderBy[A]
  def showBy[A]  = new Ops.ShowBy[A]

  def ?[A](implicit value: A): A                        = value
  def Try[A](body: => A): scala.util.Try[A]             = scala.util.Try[A](body)
  def andFalse(x: Unit): Boolean                        = false
  def andTrue(x: Unit): Boolean                         = true
  def asExpected[A](body: Any): A                       = body.asInstanceOf[A]
  def classTag[T: ClassTag] : ClassTag[T]               = implicitly[ClassTag[T]]
  def dateTime(): String                                = new java.text.SimpleDateFormat("yyyyMMdd-HH-mm-ss") format new java.util.Date
  def defaultCharset                                    = java.nio.charset.Charset.defaultCharset
  def fail(msg: String): Nothing                        = throw new RuntimeException(msg)
  def fromUTF8(xs: Array[Byte]): String                 = new String(scala.io.Codec fromUTF8 xs)
  def javaClassOf[T: scala.reflect.ClassTag] : Class[T] = classTag[T].runtimeClass.asInstanceOf[Class[T]]
  def nanoTime: Long                                    = System.nanoTime
  def nullAs[A] : A                                     = null.asInstanceOf[A]
  def printResult[A](msg: String)(result: A): A         = try result finally println(s"$msg: $result")
  def showResult[A: Show](msg: String)(result: A): A    = try result finally println("$msg: ${result.to_s}")
  def regex(re: String): Regex                          = Regex(re)

  def convertSeq[A, B](xs: List[A])(implicit conversion: A => B): List[B]     = xs map conversion
  def convertSeq[A, B](xs: Vector[A])(implicit conversion: A => B): Vector[B] = xs map conversion
  def convertSeq[A, B](xs: Seq[A])(implicit conversion: A => B): Seq[B]       = xs map conversion

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

  // String arrangements.
  def tabular[A](rows: Seq[A], join: Seq[String] => String)(columns: (A => String)*): String = {
    val cols   = columns.toVector
    val widths = cols map (f => rows map f map (_.length) max)
    def one(width: Int, value: String): String = (
      if (width == 0 || value == "") ""
      else ("%-" + width + "s") format value
    )
    rows map (row => join(widths zip (cols map (_ apply row)) map { case (w, v) => one(w, v) })) mkString "\n"
  }
}

object Ops {
  /** Working around 2.10 value class bug. */
  private def newOrdering[A](f: (A, A) => Cmp): Ordering[A] =
    new Ordering[A] { def compare(x: A, y: A): Int = f(x, y).intValue }

  // Can't make the parameter private until 2.11.
  final class ApiAnyOps[A](val __psp_x: A) extends AnyVal {
    // "Maybe we can enforce good programming practice with annoyingly long method names."
    def castTo[U] : U = __psp_x.asInstanceOf[U]
    def toRef: AnyRef = castTo[AnyRef]
    def reflect[B](m: java.lang.reflect.Method)(args: Any*): B = m.invoke(__psp_x, args map (_.asInstanceOf[AnyRef]): _*).asInstanceOf[B]
    def isNull: Boolean = toRef eq null

    // The famed forward pipe.
    @inline def |>[B](f: A => B): B       = f(__psp_x)
    @inline def doto(f: A => Unit): A     = try __psp_x finally f(__psp_x)
    @inline def sideEffect(body: Unit): A = __psp_x

    // Calling eq on Anys.
    def ref_==(y: Any): Boolean = toRef eq y.asInstanceOf[AnyRef]
    def id_## : Int             = System identityHashCode __psp_x

    def maybe[B](pf: PartialFunction[A, B]): Option[B] = pf lift __psp_x
    def try_s[A1 >: A](implicit shows: Show[A1] = null): String = if (shows == null) any_s else shows show __psp_x
    def any_s: String = __psp_x match {
      case x: ShowDirect => x.to_s
      case _             => "" + __psp_x
    }
  }
  final class OrderOps[A](val __psp_ord: Order[A]) extends AnyVal {
    def reverse: Order[A]          = Order[A]((x, y) => __psp_ord.compare(x, y).flip)
    def toOrdering: Ordering[A]    = newOrdering[A](__psp_ord.compare)
    def on[B](f: B => A): Order[B] = Order[B]((x, y) => __psp_ord.compare(f(x), f(y)))
  }
  final class OptionOps[A](val __psp_x: Option[A]) extends AnyVal {
    def or(alt: => A): A         = __psp_x getOrElse alt
    def | (alt: => A): A         = __psp_x getOrElse alt
    def ||(alt: => A): Option[A] = __psp_x orElse Some(alt)
  }
  final class TryOps[A](val __psp_x: scala.util.Try[A]) extends AnyVal {
    def | (expr: => A): A = __psp_x match {
      case scala.util.Failure(_) => expr
      case scala.util.Success(x) => x
    }
    def ||(expr: => A): scala.util.Try[A] = __psp_x match {
      case x @ scala.util.Success(_) => x
      case scala.util.Failure(_)     => scala.util.Try(expr)
    }
    def fold[B](f: A => B, g: Throwable => B): B = __psp_x match {
      case scala.util.Success(x) => f(x)
      case scala.util.Failure(t) => g(t)
    }
  }
  final class ShowInterpolator(val __psp_sc: StringContext) extends AnyVal {
    /** The type of args forces all the interpolation variables to
     *  be of a type which is implicitly convertible to Shown, which
     *  means they have a Show[A] in scope.
     */
    def show(args: Shown*): String  = StringContext(__psp_sc.parts: _*).raw(args: _*)
    def shown(args: Shown*): Shown  = Shown(show(args: _*))
    def pp(args: TryShown*): String = StringContext(__psp_sc.parts: _*).raw(args: _*)
  }
  final class ShowDirectOps(val __psp_x: ShowDirect) extends AnyVal {
    /** Java-style String addition without abandoning type safety.
     */
    def + (that: ShowDirect): ShowDirect = Shown(__psp_x.to_s + that.to_s)
    def + [A: Show](that: A): ShowDirect = Shown(__psp_x.to_s + (implicitly[Show[A]] show that))
  }

  /** The funny parameter names are because they can't be made private in 2.10
   *  due to value class limitations, but that leaves them eligible to drive
   *  implicit conversions to these classes.
   */
  final class EqClass[-A](val __psp_f: (A, A) => Boolean) extends AnyVal with Eq[A] {
    def equiv(x: A, y: A): Boolean = __psp_f(x, y)
  }
  final class OrderClass[-A](val __psp_f: (A, A) => Cmp) extends AnyVal with Order[A] {
    def compare(x: A, y: A): Cmp = __psp_f(x, y)
  }
  final class ShowClass[-A](val __psp_f: A => String) extends  AnyVal with Show[A] {
    def show(x: A): String = __psp_f(x)
  }
  final class ReadClass[A](val __psp_f: String => A) extends AnyVal with Read[A] {
    def read(x: String): A = __psp_f(x)
  }
  final class HashEqClass[-A](cmp: (A, A) => Boolean, h: A => Int) extends HashEq[A] {
    def equiv(x: A, y: A) = cmp(x, y)
    def hash(x: A)        = h(x)
  }

  final class OrderBy[A] { def apply[B](f: A => B)(implicit ord: Order[B]): Order[A] = Order[A]((x, y) => ord.compare(f(x), f(y))) }
  final class EqBy[A]    { def apply[B](f: A => B)(implicit equiv: Eq[B]): Eq[A]     = Eq[A]((x, y) => equiv.equiv(f(x), f(y))) }
  final class ShowBy[A]  { def apply[B](f: A => B)(implicit show: Show[B]): Show[A]  = Show[A](x => show show f(x)) }
}
