package psp

import java.nio.{ file => jnf }
import jnf.{ attribute => jnfa }
import scala.collection.{ generic => scg, mutable => scm, immutable => sci }
import scala.sys.process.{ Process, ProcessBuilder }
import psp.std.api._

package object std extends psp.std.PackageImplicits with psp.std.api.Aliases {
  @inline final implicit def arrowAssocInt(x: Int): Ops.ArrowAssocInt             = new Ops.ArrowAssocInt(x)
  @inline final implicit def arrowAssocLong(x: Long): Ops.ArrowAssocLong          = new Ops.ArrowAssocLong(x)
  @inline final implicit def arrowAssocDouble(x: Double): Ops.ArrowAssocDouble    = new Ops.ArrowAssocDouble(x)
  @inline final implicit def arrowAssocChar(x: Char): Ops.ArrowAssocChar          = new Ops.ArrowAssocChar(x)
  @inline final implicit def arrowAssocBoolean(x: Boolean): Ops.ArrowAssocBoolean = new Ops.ArrowAssocBoolean(x)

  final case object ConstantTrue extends Predicate[Any] { def apply(x: Any): Boolean = true }
  final case object ConstantFalse extends Predicate[Any] { def apply(x: Any): Boolean = false }

  implicit def identityAlgebra : BooleanAlgebra[Boolean]          = Algebras.Identity
  implicit def predicateAlgebra[A] : BooleanAlgebra[Predicate[A]] = new Algebras.Predicate[A]
  implicit def scalaSetAlgebra[A] : BooleanAlgebra[sciSet[A]]     = new Algebras.ScalaSet[A]

  final val PolicyList = psp.std.linear.List
  type PolicyList[A]   = psp.std.linear.List[A]

  final val NoSize: Size         = Size.undefined
  final val NoPath: Path         = path("")
  final val NoFile: jFile        = jFile("")
  final val NoUri: jUri          = jUri("")
  final val NoFileTime: FileTime = jnfa.FileTime fromMillis MinLong
  final val EOL                  = sys.props.getOrElse("line.separator", "\n")
  final val MaxInt               = Int.MaxValue
  final val MinInt               = Int.MinValue
  final val MaxLong              = Long.MaxValue
  final val MinLong              = Long.MinValue
  final val NoIndex              = Index.undefined
  final val NoNth                = Nth.undefined
  final val NumericRange         = sci.NumericRange
  final val ScalaNil             = sci.Nil
  final val CTag                 = scala.reflect.ClassTag

  def unknownSize: SizeInfo = SizeInfo.Unknown

  type ForeachableType[A0, Repr, CC0[X]] = Foreachable[Repr] {
    type A = A0
    type CC[B] = CC0[B]
  }
  type DirectAccessType[A0, Repr, CC0[X]] = DirectAccess[Repr] {
    type A = A0
    type CC[B] = CC0[B]
  }


  // Operations involving the filesystem.
  def javaHome: jFile                                       = jFile(scala.util.Properties.javaHome)
  def fileSeparator                                         = java.io.File.separator
  def defaultCharset                                        = java.nio.charset.Charset.defaultCharset
  def path(s: String, ss: String*): Path                    = ss.foldLeft(jnf.Paths get s)(_ resolve _)
  def newTempDir(prefix: String, attrs: AnyFileAttr*): Path = jnf.Files.createTempDirectory(prefix, attrs: _*)

  // Operations involving external processes.
  def newProcess(line: String): ProcessBuilder      = Process(line)
  def newProcess(args: Seq[String]): ProcessBuilder = Process(args)
  def executeLine(line: String): Int                = Process(line).!
  def execute(args: String*): Int                   = Process(args.toSeq).!
  def openInApp(app: String, file: jFile): Unit     = execute("open", "-a", app, file.getAbsolutePath)
  def openSafari(file: jFile): Unit                 = openInApp("Safari", file)
  def openChrome(file: jFile): Unit                 = openInApp("Google Chrome", file)

  def eqBy[A]       = new Ops.EqBy[A]
  def orderBy[A]    = new Ops.OrderBy[A]
  def showBy[A]     = new Ops.ShowBy[A]
  def hashEq[A: Eq] = HashEq native ?[Eq[A]]

  // Operations involving encoding/decoding of string data.
  def utf8(xs: Array[Byte]): Utf8   = new Utf8(xs)
  def decodeName(s: String): String = scala.reflect.NameTransformer decode s
  def encodeName(s: String): String = scala.reflect.NameTransformer encode s

  // Operations involving time and date.
  def formattedDate(format: String)(date: jDate): String = new java.text.SimpleDateFormat(format) format date
  def dateTime(): String                                 = formattedDate("yyyyMMdd-HH-mm-ss")(new jDate)
  def nanoTime: Long                                     = System.nanoTime
  def milliTime: Long                                    = System.currentTimeMillis
  def now(): FileTime                                    = jnfa.FileTime fromMillis milliTime
  def timed[A](body: => A): A                            = nanoTime |> (start => try body finally errLog("Elapsed: %.3f ms" format (nanoTime - start) / 1e6))

  // Operations involving classes, classpaths, and classloaders.
  def classTag[T: CTag] : CTag[T]          = implicitly[CTag[T]]
  def contextLoader(): ClassLoader         = noNull(Thread.currentThread.getContextClassLoader, nullLoader)
  def loaderOf[A: ClassTag] : ClassLoader  = noNull(jClassOf[A].getClassLoader, nullLoader)
  def nullLoader(): ClassLoader            = NullClassLoader
  def resource(name: String): Array[Byte]  = Try(contextLoader) || loaderOf[this.type] fold (_ getResourceAsStream name slurp, _ => Array.empty)
  def resourceString(name: String): String = utf8(resource(name)).to_s
  def classpathSeparator                   = java.io.File.pathSeparator

  // Operations involving Null, Nothing, and casts.
  def fail(msg: String): Nothing           = throw new RuntimeException(msg)
  def failEmpty(op: String): Nothing       = throw new NoSuchElementException(s"$op on empty collection")
  def noNull[A](value: A, orElse: => A): A = if (value == null) orElse else value
  def nullAs[A] : A                        = asExpected[A](null)
  def asExpected[A](body: Any): A          = body.castTo[A]

  def ?[A](implicit value: A): A                         = value
  def Try[A](body: => A): Try[A]                         = scala.util.Try[A](body)
  def andFalse(x: Unit): Boolean                         = false
  def andTrue(x: Unit): Boolean                          = true
  def each[A](xs: GTOnce[A]): Foreach[A]                 = Foreach traversable xs
  def index(x: Int): Index                               = Index(x)
  def indexRange(start: Int, end: Int): IndexRange       = IndexRange.until(Index(start), Index(end))
  def labelpf[T, R](label: String)(pf: T ?=> R): T ?=> R = new LabeledPartialFunction(pf, label)
  def nth(x: Int): Nth                                   = Nth(x)
  def nullStream(): InputStream                          = NullInputStream
  def offset(x: Int): Offset                             = Offset(x)
  def ordering[A: Order] : Ordering[A]                   = ?[Order[A]].toScalaOrdering
  def regex(re: String): Regex                           = Regex(re)

  def printResult[A](msg: String)(result: A): A      = try result finally println(s"$msg: $result")
  def showResult[A: Show](msg: String)(result: A): A = try result finally println(show"$msg: $result")
  def errLog(msg: String): Unit                      = Console.err println msg

  def convertSeq[A, B](xs: List[A])(implicit conversion: A => B): List[B]     = xs map conversion
  def convertSeq[A, B](xs: Vector[A])(implicit conversion: A => B): Vector[B] = xs map conversion
  def convertSeq[A, B](xs: scSeq[A])(implicit conversion: A => B): scSeq[B]   = xs map conversion

  def scmSeq[A](xs: A*): scmSeq[A]             = scm.Seq(xs: _*)
  def scmSet[A](xs: A*): scmSet[A]             = scm.Set(xs: _*)
  def scmMap[K, V](kvs: (K, V)*): scmMap[K, V] = scm.Map[K, V](kvs: _*)
  def sciSeq[A](xs: A*): sciSeq[A]             = sci.Seq(xs: _*)
  def sciSet[A](xs: A*): sciSet[A]             = sci.Set(xs: _*)
  def sciMap[K, V](kvs: (K, V)*): sciMap[K, V] = sci.Map[K, V](kvs: _*)

  def setBuilder[A](xs: A*): Builder[A, sciSet[A]]        = sci.Set.newBuilder[A] ++= xs
  def listBuilder[A](xs: A*): Builder[A, List[A]]         = sci.List.newBuilder[A] ++= xs
  def arrayBuilder[A: CTag](xs: A*): Builder[A, Array[A]] = scala.Array.newBuilder[A] ++= xs
  def vectorBuilder[A](xs: A*): Builder[A, Vector[A]]     = sci.Vector.newBuilder[A] ++= xs

  // Java.
  def jMap[K, V](xs: (K, V)*): jMap[K, V] = new jHashMap[K, V] doto (b => for ((k, v) <- xs) b.put(k, v))
  def jSet[A](xs: A*): jSet[A]            = new jHashSet[A] doto (b => xs foreach b.add)
  def jList[A](xs: A*): jArrayList[A]     = new jArrayList[A] doto (b => xs foreach b.add)
  def jFile(s: String): jFile             = path(s).toFile
  def jUri(x: String): jUri               = java.net.URI create x
  def jUrl(x: String): jUrl               = jUri(x).toURL
  def jClassOf[T: CTag] : Class[_ <: T]   = classTag[T].runtimeClass.castTo[Class[_ <: T]]

  // OrderedMap is our own creation since SortedMap is way overspecified
  // and LinkedHashMap is too slow and only comes in a mutable variety.
  def orderedMap[K, V](kvs: (K, V)*): OrderedMap[K, V]                 = new OrderedMap[K, V](kvs map (_._1), kvs.toMap)
  def orderedMap[K, V](keys: Seq[K], map: Map[K, V]): OrderedMap[K, V] = new OrderedMap[K, V](keys, map)

  def show[A: Show] : Show[A]        = ?
  // def readInto[A] : Read.ReadInto[A] = Read.into[A]

  def precise(n: Int): Precise = Precise(Size(n))
  def bounded(lo: Size, hi: SizeInfo): SizeInfo = hi match {
    case hi: Atomic     => bounded(lo, hi)
    case Bounded(_, hi) => bounded(lo, hi)
  }
  def bounded(lo: SizeInfo, hi: SizeInfo): SizeInfo = lo match {
    case Precise(lo)    => bounded(lo, hi)
    case Bounded(lo, _) => bounded(lo, hi)
    case Infinite       => Infinite
  }
  def bounded(lo: Size, hi: Atomic): SizeInfo = hi match {
    case Precise(n) if n < lo  => SizeInfo.Empty
    case Precise(n) if n == lo => hi
    case _                     => Bounded(lo, hi)
  }


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
