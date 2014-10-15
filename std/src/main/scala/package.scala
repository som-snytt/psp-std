package psp

import java.nio.{ file => jnf }
import jnf.{ attribute => jnfa }
import scala.{ collection => sc }
import scala.collection.{ generic => scg, mutable => scm, immutable => sci }
import scala.sys.process.{ Process, ProcessBuilder }
import psp.std.api._

package object std extends psp.std.PackageImplicits {
  type sMap[K, +V] = sciMap[K, V]
  type sList[+A]   = sciList[A]
  type sSet[A]     = sciSet[A]
  type sVector[+A] = sciVector[A]
  type sSeq[+A]    = scSeq[A]

  type pSeq[+A]    = Foreach[A]
  type pVector[+A] = Direct[A]

  // Inlinable.
  final val InputStreamBufferSize = 8192
  final val MaxInt                = scala.Int.MaxValue
  final val MaxLong               = scala.Long.MaxValue
  final val MinInt                = scala.Int.MinValue
  final val MinLong               = scala.Long.MinValue
  final val PositiveInfinity      = scala.Double.PositiveInfinity

  // DMZ.
  // final val +:      = psp.dmz.+:
  final val :+      = psp.dmz.:+
  final val ::      = psp.dmz.::
  final val Array   = psp.dmz.Array
  final val Console = psp.dmz.Console
  final val List    = psp.dmz.List
  final val Map     = psp.dmz.Map
  final val Option  = psp.dmz.Option
  final val Seq     = psp.dmz.Seq
  final val Set     = psp.dmz.Set
  final val Some    = psp.dmz.Some
  final val System  = psp.dmz.System
  final val Try     = psp.dmz.Try
  final val Tuple2  = psp.dmz.Tuple2
  final val Vector  = psp.dmz.Vector
  final val math    = psp.dmz.math
  final val sys     = psp.dmz.sys
  final val Success = psp.dmz.Success
  final val Failure = psp.dmz.Failure

  final val BigDecimal      = scala.math.BigDecimal
  final val BigInt          = scala.math.BigInt
  final val ClassTag        = scala.reflect.ClassTag
  final val NameTransformer = scala.reflect.NameTransformer
  final val Nil             = sci.Nil
  final val None            = scala.None
  final val Ordering        = scala.math.Ordering
  final val PolicyList      = psp.std.linear.List
  final val StringContext   = scala.StringContext
  final val sConsole        = scala.Console
  final val scBitSet        = sc.BitSet
  final val scIndexedSeq    = sc.IndexedSeq
  final val scIterable      = sc.Iterable
  final val scIterator      = sc.Iterator
  final val scLinearSeq     = sc.LinearSeq
  final val scMap           = sc.Map
  final val scSeq           = sc.Seq
  final val scSet           = sc.Set
  final val scTraversable   = sc.Traversable
  final val sciBitSet       = sci.BitSet
  final val sciIndexedSeq   = sci.IndexedSeq
  final val sciIterable     = sci.Iterable
  final val sciLinearSeq    = sci.LinearSeq
  final val sciList         = sci.List
  final val sciMap          = sci.Map
  final val sciNumericRange = sci.NumericRange
  final val sciRange        = sci.Range
  final val sciSeq          = sci.Seq
  final val sciSet          = sci.Set
  final val sciStream       = sci.Stream
  final val sciTraversable  = sci.Traversable
  final val sciVector       = sci.Vector
  final val scmMap          = scm.Map
  final val scmSeq          = scm.Seq
  final val scmSet          = scm.Set
  final val scmWrappedArray = scm.WrappedArray

  final val ConstantTrue: Predicate[Any]  = _ => true
  final val ConstantFalse: Predicate[Any] = _ => false

  final val CTag                 = scala.reflect.ClassTag
  final val EOL                  = sys.props.getOrElse("line.separator", "\n")
  final val NoFile: jFile        = jFile("")
  final val NoPath: Path         = path("")
  final val NoUri: jUri          = jUri("")
  final val NoFileTime: FileTime = jnfa.FileTime fromMillis MinLong
  final val NoIndex              = Index.undefined
  final val NoNth                = Nth.undefined
  final val NoSize: Size         = Size.undefined

  def assert(assertion: Boolean): Unit                 = if (!assertion) assertionError("assertion failed")
  def assert(assertion: Boolean, msg: => Any): Unit    = if (!assertion) assertionError(s"assertion failed: $msg")
  def require(requirement: Boolean): Unit              = if (!requirement) illegalArgumentException("requirement failed")
  def require(requirement: Boolean, msg: => Any): Unit = if (!requirement) illegalArgumentException(s"requirement failed: $msg")
  def ??? : Nothing                                    = throw new NotImplementedError
  def identity[A](x: A): A                             = x
  def implicitly[A](implicit x: A): A                  = x
  def locally[A](x: A): A                              = x

  def echoErr[A](x: A)(implicit z: TryShow[A]): Unit     = Console echoErr (z show x)
  def println[A](x: A)(implicit z: TryShow[A]): Unit     = Console echoOut (z show x)

  type PolicyList[A]   = psp.std.linear.List[A]

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

  def eqBy[A]     = new ops.EqBy[A]
  def hashBy[A]   = new ops.HashBy[A]
  def hashEqBy[A] = new ops.HashEqBy[A]
  def orderBy[A]  = new ops.OrderBy[A]
  def showBy[A]   = new ops.ShowBy[A]

  // Operations involving encoding/decoding of string data.
  def utf8(xs: Array[Byte]): Utf8   = new Utf8(xs)
  def decodeName(s: String): String = scala.reflect.NameTransformer decode s
  def encodeName(s: String): String = scala.reflect.NameTransformer encode s

  // Operations involving time and date.
  def formattedDate(format: String)(date: jDate): String = new java.text.SimpleDateFormat(format) format date
  def dateTime(): String                                 = formattedDate("yyyyMMdd-HH-mm-ss")(new jDate)
  def now(): FileTime                                    = jnfa.FileTime fromMillis milliTime
  def timed[A](body: => A): A                            = nanoTime |> (start => try body finally errLog("Elapsed: %.3f ms" format (nanoTime - start) / 1e6))

  // Operations involving classes, classpaths, and classloaders.
  def classTag[T: CTag] : CTag[T]          = implicitly[CTag[T]]
  def contextLoader(): ClassLoader         = noNull(currentThread.getContextClassLoader, nullLoader)
  def loaderOf[A: ClassTag] : ClassLoader  = noNull(jClassOf[A].getClassLoader, nullLoader)
  def nullLoader(): ClassLoader            = NullClassLoader
  def resource(name: String): Array[Byte]  = Try(contextLoader) || loaderOf[this.type] fold (_ getResourceAsStream name slurp, _ => Array.empty)
  def resourceString(name: String): String = utf8(resource(name)).to_s

  // Operations involving Null, Nothing, and casts.
  def fail(msg: String): Nothing           = throw new RuntimeException(msg)
  def failEmpty(op: String): Nothing       = throw new NoSuchElementException(s"$op on empty collection")
  def noNull[A](value: A, orElse: => A): A = if (value == null) orElse else value
  def nullAs[A] : A                        = asExpected[A](null)
  def asExpected[A](body: Any): A          = body.castTo[A]

  def ?[A](implicit value: A): A                         = value
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
  def orderedMap[K, V](kvs: (K, V)*): OrderedMap[K, V]                       = new OrderedMap[K, V](kvs.toVector map (_._1), kvs.toMap)
  def orderedMap[K, V](keys: sciSeq[K], map: sciMap[K, V]): OrderedMap[K, V] = new OrderedMap[K, V](keys, map)

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

  def newCmp(difference: Long): Cmp = if (difference < 0) Cmp.LT else if (difference > 0) Cmp.GT else Cmp.EQ
}
