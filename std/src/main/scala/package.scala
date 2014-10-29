package psp

import java.nio.file.Paths
import java.nio.file.{ attribute => jnfa }
import scala.{ collection => sc }
import sc.{ mutable => scm, immutable => sci }
import psp.std.api._
import psp.std.lowlevel._
import psp.std.StdShow._

package object std extends psp.std.StdPackage {
  type pVector[+A]       = Direct[A]
  type pList[A]          = PolicyList[A]
  type inSet[A]          = IntensionalSet[A]
  type exSet[A]          = ExtensionalSet[A]
  type inMap[K, V]       = IntensionalMap[K, V]
  type exMap[K, V]       = ExtensionalMap[K, V]
  type exSeq[+A]         = Each[A]
  type pMutableMap[K, V] = PolicyMutableMap[K, V]
  type DocSeq            = Each[Doc]

  // Inlinable.
  final val InputStreamBufferSize = 8192
  final val MaxInt                = scala.Int.MaxValue
  final val MaxLong               = scala.Long.MaxValue
  final val MinInt                = scala.Int.MinValue
  final val MinLong               = scala.Long.MinValue
  final val MaxIndex              = Index(MaxLong)
  final val PositiveInfinity      = scala.Double.PositiveInfinity

  // DMZ.
  final val ::      = psp.dmz.::
  final val Array   = psp.dmz.Array
  final val Console = psp.dmz.Console
  final val Failure = psp.dmz.Failure
  final val Set     = psp.dmz.Set
  final val Success = psp.dmz.Success
  final val System  = psp.dmz.System
  final val Try     = psp.dmz.Try
  final val math    = psp.dmz.math
  final val sys     = psp.dmz.sys

  final val BigDecimal      = scala.math.BigDecimal
  final val BigInt          = scala.math.BigInt
  final val ClassTag        = scala.reflect.ClassTag
  final val NameTransformer = scala.reflect.NameTransformer
  final val Nil             = sci.Nil
  final val None            = scala.None
  final val Option          = scala.Option
  final val Ordering        = scala.math.Ordering
  final val Some            = scala.Some
  final val StringContext   = scala.StringContext
  final val scIterator      = sc.Iterator
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

  final val ConstantTrue         = (x: Any) => true
  final val ConstantFalse        = (x: Any) => false
  final val CTag                 = scala.reflect.ClassTag
  final val EOL                  = sys.props.getOrElse("line.separator", "\n")
  final val NoFile: jFile        = jFile("")
  final val NoPath: Path         = path("")
  final val NoUri: jUri          = jUri("")
  final val NoFileTime: FileTime = jnfa.FileTime fromMillis MinLong
  final val NoIndex              = Index.undefined
  final val NoNth                = Nth.undefined

  // Methods similar to the more useful ones in scala's Predef.
  def ??? : Nothing                                                = throw new scala.NotImplementedError
  def assert(assertion: Boolean): Unit                             = if (!assertion) assertionError("assertion failed")
  def assert(assertion: Boolean, msg: => Any): Unit                = if (!assertion) assertionError(s"assertion failed: $msg")
  def asserting[A](x: A)(assertion: => Boolean, msg: => String): A = x sideEffect assert(assertion, msg)
  def echoErr[A: TryShow](x: A): Unit                              = Console echoErr pp"$x"
  def identity[A](x: A): A                                         = x
  def implicitly[A](implicit x: A): A                              = x
  def implicitOrZero[A: Zero](implicit value: A = nullAs[A]): A    = if (value.isNull) zero[A] else value
  def printResult[A: TryShow](msg: String)(result: A): A           = result doto (r => println(pp"$msg: $r"))
  def println[A: TryShow](x: A): Unit                              = Console echoOut pp"$x"
  def require(requirement: Boolean): Unit                          = if (!requirement) illegalArgumentException("requirement failed")
  def require(requirement: Boolean, msg: => Any): Unit             = if (!requirement) illegalArgumentException(s"requirement failed: $msg")
  def showResult[A: TryShow](msg: String)(result: A): A            = result doto (r => println(pp"$msg: $r"))

  // Operations involving classes, classpaths, and classloaders.
  def classLoaderOf[A: CTag](): ClassLoader = classOf[A].getClassLoader
  def classOf[A: CTag](): Class[_ <: A]     = classTag[A].runtimeClass.castTo[Class[_ <: A]]
  def classTag[T: CTag] : CTag[T]           = implicitly[CTag[T]]
  def loaderOf[A: CTag] : ClassLoader       = noNull(classLoaderOf[A], nullLoader)
  def nullLoader(): ClassLoader             = NullClassLoader
  def pClassOf[A: CTag](): PolicyClass      = new PolicyClass(classOf[A])
  def resource(name: String): Array[Byte]   = Try(noNull(contextClassLoader, nullLoader)) || loaderOf[this.type] fold (_ => Array.empty, _ getResourceAsStream name slurp)
  def resourceString(name: String): String  = utf8(resource(name)).to_s

  def path(s: String, ss: String*): Path          = ss.foldLeft(Paths get s)(_ resolve _)

  def eqBy[A]     = new ops.EqBy[A]
  def hashBy[A]   = new ops.HashBy[A]
  def hashEqBy[A] = new ops.HashEqBy[A]
  def orderBy[A]  = new ops.OrderBy[A]
  def showBy[A]   = new ops.ShowBy[A]

  // Operations involving encoding/decoding of string data.
  def utf8(xs: Array[Byte]): Utf8   = new Utf8(xs)
  def decodeName(s: String): String = s.mapSplit('.')(NameTransformer.decode)
  def encodeName(s: String): String = s.mapSplit('.')(NameTransformer.encode)

  // Operations involving time and date.
  def formattedDate(format: String)(date: jDate): String = new java.text.SimpleDateFormat(format) format date
  def dateTime(): String                                 = formattedDate("yyyyMMdd-HH-mm-ss")(new jDate)
  def now(): FileTime                                    = jnfa.FileTime fromMillis milliTime
  def timed[A](body: => A): A                            = nanoTime |> (start => try body finally echoErr("Elapsed: %.3f ms" format (nanoTime - start) / 1e6))

  // Operations involving Null, Nothing, and casts.
  def abortTrace(msg: String): Nothing     = new RuntimeException(msg) |> (ex => try throw ex finally ex.printStackTrace)
  def abort(msg: String): Nothing          = runtimeException(msg)
  def noNull[A](value: A, orElse: => A): A = if (value == null) orElse else value
  def nullAs[A] : A                        = asExpected[A](null)
  def asExpected[A](body: Any): A          = body.castTo[A]

  def partial[A, B](f: A ?=> B): A ?=> B                = f
  def ?[A](implicit value: A): A                        = value
  def andFalse(x: Unit): Boolean                        = false
  def andTrue(x: Unit): Boolean                         = true
  def direct[A](xs: A*): Direct[A]                      = new Direct.FromScala(xs.toVector)
  def each[A](xs: sCollection[A]): Each[A]           = fromScala(xs)
  def indexRange(start: Int, end: Int): IndexRange      = IndexRange(start, end)
  def intRange(start: Int, end: Int): ExclusiveIntRange = ExclusiveIntRange(start, end)
  def nthRange(start: Int, end: Int): ExclusiveIntRange = ExclusiveIntRange(start, end + 1)
  def nullStream(): InputStream                         = NullInputStream
  def offset(x: Int): Offset                            = Offset(x)
  def option[A](p: Boolean, x: => A): Option[A]         = if (p) Some(x) else None
  def ordering[A: Order] : Ordering[A]                  = ?[Order[A]].toScalaOrdering
  def regex(re: String): Regex                          = Regex(re)
  def show[A: Show] : Show[A]                           = ?
  def zero[A](implicit z: Zero[A]): A                   = z.zero

  def fromScala[A](xs: sCollection[A]): AnyView[A] = xs match {
    case xs: sciIndexedSeq[_] => new Direct.FromScala(xs) m
    case xs: sciLinearSeq[_]  => new PolicyList.FromScala(xs) m
    case xs: sciSet[_]        => new PolicySet.FromScala(xs) m
    case _                    => new Each.FromScala(xs) m
  }
  def fromJava[A](xs: jIterable[A]): AnyView[A] = xs match {
    case xs: jList[_] => new Direct.FromJava[A](xs) m
    case xs: jSet[_]  => new PolicySet.FromJava[A](xs) m
    case xs           => new Each.FromJava[A](xs) m
  }
  def fromElems[A](xs: A*): Direct[A] = new Direct.FromScala(xs.toVector)

  def arrayBuilder[A: CTag](xs: A*): scmBuilder[A, Array[A]] = scala.Array.newBuilder[A] ++= xs
  def vectorBuilder[A](xs: A*): scmBuilder[A, sciVector[A]]  = sciVector.newBuilder[A] ++= xs

  // Java.
  def jConcurrentMap[K, V](xs: (K, V)*): jConcurrentMap[K, V] = new jConcurrentHashMap[K, V] doto (b => for ((k, v) <- xs) b.put(k, v))
  def jFile(s: String): jFile                                 = path(s).toFile
  def jList[A](xs: A*): jList[A]                              = java.util.Arrays.asList(xs: _*)
  def jMap[K, V](xs: (K, V)*): jMap[K, V]                     = new jHashMap[K, V] doto (b => for ((k, v) <- xs) b.put(k, v))
  def jSet[A](xs: A*): jSet[A]                                = new jHashSet[A] doto (b => xs foreach b.add)
  def jUri(x: String): jUri                                   = java.net.URI create x
  def jUrl(x: String): jUrl                                   = jUri(x).toURL

  object PSeq {
    def unapplySeq[A](xs: Each[A]): scala.Some[scSeq[A]] = Some(xs.seq)
  }
  object Pair {
    def apply[R, A, B](x: A, y: B)(implicit z: PairUp[R, A, B]): R                = z.create(x, y)
    def unapply[R, A, B](x: R)(implicit z: PairDown[R, A, B]): scala.Some[(A, B)] = Some((z left x, z right x))
  }

  def PairDown[R, A, B](l: R => A, r: R => B): PairDown[R, A, B] = new PairDown[R, A, B] {
    def left(x: R)  = l(x)
    def right(x: R) = r(x)
  }
  def PairUp[R, A, B](f: (A, B) => R): PairUp[R, A, B] = new PairUp[R, A, B] { def create(x: A, y: B) = f(x, y) }

  def exMap[K: HashEq, V](xs: (K, V)*): exMap[K, V]     = xs.m.pmap
  def exSeq[A](xs: A*): Each[A]                         = xs.m.pseq
  def exSet[A: HashEq](xs: A*): exSet[A]                = xs.m.pset
  def exView[A](xs: A*): View[A]                        = Direct[A](xs: _*).m
  def inSet[A: HashEq](p: Predicate[A]): inSet[A]       = p.inSet
  def inView[A](mf: Suspended[A]): View[A]              = Each[A](mf).m

  def pList[A](xs: A*): pList[A]                        = xs.m.plist
  def pMutableMap[K, V](xs: (K, V)*): pMutableMap[K, V] = new PolicyMutableMap(jConcurrentMap(xs: _*))

  def indexedView[A, Repr](size: Precise, f: Index => A): IndexedView[A, Repr] = new IndexedView[A, Repr](Direct.pure(size, f))

  def newPartial[K, V](p: K => Boolean, f: K => V): K ?=> V = { case x if p(x) => f(x) }
  def newCmp(difference: Long): Cmp                         = if (difference < 0) Cmp.LT else if (difference > 0) Cmp.GT else Cmp.EQ
  def newArray[A: CTag](size: Precise): Array[A]            = new Array[A](size.intSize)
  def newSize(n: Long): Precise                             = if (n < 0) Precise(0) else if (n > MaxInt) Precise(n) else Precise(n.toInt)

  def handleSlice[A](xs: Each[A], range: IndexRange, f: A => Unit): IndexRange = xs match {
    case xs: Direct[A] => directlySlice(xs, range, f)
    case _             => linearlySlice(xs, range, f)
  }
  def linearlySlice[A](xs: Each[A], range: IndexRange, f: A => Unit): IndexRange = {
    var dropping = range.precedingSize
    var remaining = range.size
    var index = Index(0)
    xs foreach { x =>
      index += 1
      if (dropping > 0)
        dropping = dropping - 1
      else if (remaining > 0)
        try f(x) finally remaining -= 1
      else
        return index until range.end
    }
    index until range.end
  }
  def directlySlice[A](xs: Direct[A], range: IndexRange, f: A => Unit): IndexRange = {
    var index = Index(0)
    xs foreach { x =>
      if (range containsIndex index) f(x)
      index += 1
    }
    index until range.end
  }
}
