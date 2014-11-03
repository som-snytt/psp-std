package psp

import java.nio.file.Paths
import java.nio.file.{ attribute => jnfa }
import scala.{ collection => sc }
import sc.{ mutable => scm, immutable => sci }
import psp.std.api._
import psp.std.lowlevel._
import psp.std.StdShow._

package std {
  trait Assertions {
    def failed(msg: => String): Unit
    def assert(assertion: Boolean, msg: => String): Unit = if (!assertion) failed(msg)
  }
  object Assertions {
    private[this] var instance: Assertions = DefaultAssertions
    def using[A](x: Assertions)(assertion: => Boolean, msg: => String): Unit = {
      val saved = instance
      instance = x
      try instance.assert(assertion, msg) finally instance = saved
    }
    implicit object DefaultAssertions extends Assertions {
      def failed(msg: => String): Unit = assertionError(msg)
    }
  }

  object ImmediateTraceAssertions extends Assertions {
    def failed(msg: => String): Unit = {
      val t = new AssertionError(msg)
      t.printStackTrace
      throw t
    }
  }
}

package object std extends psp.std.StdPackage {
  type pList[A]    = PolicyList[A]
  type exSeq[+A]   = Each[A]
  type DocSeq      = Each[Doc]

  /** Scala, so aggravating.
   *  [error] could not find implicit value for parameter equiv: psp.std.api.Eq[psp.tests.Pint => psp.std.Boolean]
   *  The parameter can be given explicitly, it just won't be found unless the function type is invariant.
   *  The same issue arises with intensional sets.
   */
  type InvariantPredicate[A] = A => Boolean
  type InvariantInSet[A]     = InSet[A]
  type View2D[+A]            = View[View[A]]

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
  final val scSeq           = sc.Seq
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
  def ??? : Nothing                                                                        = throw new scala.NotImplementedError
  def assert(assertion: => Boolean)(implicit z: Assertions): Unit                          = Assertions.using(z)(assertion, "assertion failed")
  def assert(assertion: => Boolean, msg: => Any)(implicit z: Assertions): Unit             = Assertions.using(z)(assertion, s"assertion failed: $msg")
  def asserting[A](x: A)(assertion: => Boolean, msg: => String)(implicit z: Assertions): A = x sideEffect assert(assertion, msg)
  def echoErr[A: TryShow](x: A): Unit                                                      = Console echoErr pp"$x"
  def identity[A](x: A): A                                                                 = x
  def implicitly[A](implicit x: A): A                                                      = x
  def printResult[A: TryShow](msg: String)(result: A): A                                   = result doto (r => println(pp"$msg: $r"))
  def printResultIf[A: TryShow : Eq](show: A, msg: String)(result: A): A                   = result doto (r => if (r === show) println(pp"$msg: $r"))
  def println[A: TryShow](x: A): Unit                                                      = Console echoOut pp"$x"
  def require(requirement: Boolean): Unit                                                  = if (!requirement) illegalArgumentException("requirement failed")
  def require(requirement: Boolean, msg: => Any): Unit                                     = if (!requirement) illegalArgumentException(s"requirement failed: $msg")
  def showResult[A: TryShow](msg: String)(result: A): A                                    = result doto (r => println(pp"$msg: $r"))

  // Operations involving classes, classpaths, and classloaders.
  def classLoaderOf[A: CTag](): ClassLoader = classOf[A].getClassLoader
  def classOf[A: CTag](): Class[_ <: A]     = classTag[A].runtimeClass.castTo[Class[_ <: A]]
  def classTag[T: CTag] : CTag[T]           = implicitly[CTag[T]]
  def loaderOf[A: CTag] : ClassLoader       = noNull(classLoaderOf[A], nullLoader)
  def nullLoader(): ClassLoader             = NullClassLoader
  def pClassOf[A: CTag](): PolicyClass      = new PolicyClass(classOf[A])
  def resource(name: String): Array[Byte]   = Try(noNull(contextClassLoader, nullLoader)) || loaderOf[this.type] fold (_ => Array.empty, _ getResourceAsStream name slurp)
  def resourceString(name: String): String  = utf8(resource(name)).to_s
  def classFilter[A: CTag] : Any ?=> A      = newPartial(_.isClass[A], _.castTo[A])

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

  def ?[A](implicit value: A): A                        = value
  def andFalse(x: Unit, xs: Unit*): Boolean             = false
  def andTrue(x: Unit, xs: Unit*): Boolean              = true
  def direct[A](xs: A*): Direct[A]                      = new Direct.FromScala(xs.toVector)
  def each[A](xs: sCollection[A]): Each[A]              = fromScala(xs)
  def indexRange(start: Int, end: Int): IndexRange      = IndexRange(start, end)
  def intRange(start: Int, end: Int): ExclusiveIntRange = ExclusiveIntRange(start, end)
  def nthRange(start: Int, end: Int): ExclusiveIntRange = ExclusiveIntRange(start, end + 1)
  def nullStream(): InputStream                         = NullInputStream
  def offset(x: Int): Offset                            = Offset(x)
  def option[A](p: Boolean, x: => A): Option[A]         = if (p) Some(x) else None
  def ordering[A: Order] : Ordering[A]                  = ?[Order[A]].toScalaOrdering
  def partial[A, B](f: A ?=> B): A ?=> B                = f
  def regex(re: String): Regex                          = Regex(re)
  def show[A: Show] : Show[A]                           = ?
  def zero[A](implicit z: Zero[A]): A                   = z.zero
  def emptyValue[A](implicit z: Empty[A]): A            = z.empty

  def spawn[A](body: => A): Unit = {
    val t = new Thread() { override def run(): Unit = body }
    t setDaemon true
    t.start()
  }

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

  // Java.
  def jConcurrentMap[K, V](xs: (K, V)*): jConcurrentMap[K, V] = new jConcurrentHashMap[K, V] doto (b => for ((k, v) <- xs) b.put(k, v))
  def jFile(s: String): jFile                                 = path(s).toFile
  def jList[A](xs: A*): jList[A]                              = java.util.Arrays.asList(xs: _*)
  def jMap[K, V](xs: (K, V)*): jMap[K, V]                     = new jHashMap[K, V] doto (b => for ((k, v) <- xs) b.put(k, v))
  def jSet[A](xs: A*): jSet[A]                                = new jHashSet[A] doto (b => xs foreach b.add)
  def jUri(x: String): jUri                                   = java.net.URI create x
  def jUrl(x: String): jUrl                                   = jUri(x).toURL

  def fst[A, B](x: scala.Product2[A, B]): A = x._1
  def snd[A, B](x: scala.Product2[A, B]): B = x._2

  def PairDown[R, A, B](l: R => A, r: R => B): PairDown[R, A, B] = new PairDown[R, A, B] {
    def left(x: R)  = l(x)
    def right(x: R) = r(x)
  }
  def PairUp[R, A, B](f: (A, B) => R): PairUp[R, A, B] = new PairUp[R, A, B] { def create(x: A, y: B) = f(x, y) }

  def exMap[K: HashEq, V](xs: (K, V)*): ExMap[K, V]         = xs.m.pmap
  def exSeq[A](xs: A*): Each[A]                             = xs.m.pseq
  def exSet[A: HashEq](xs: A*): ExSet[A]                    = xs.m.pset
  def exView[A](xs: A*): View[A]                            = Direct[A](xs: _*).m
  def inMap[K, V](p: Predicate[K], f: K => V): InMap[K, V]  = new IntensionalMap(inSet(p), Lookup total f)
  def inSet[A](p: Predicate[A]): InSet[A]                   = p.inSet
  def inView[A](mf: Suspended[A]): View[A]                  = Each(mf).m
  def mutableMap[K, V](xs: (K, V)*): PolicyMutableMap[K, V] = new PolicyMutableMap(jConcurrentMap(xs: _*))
  def pList[A](xs: A*): pList[A]                            = xs.m.plist

  def newPartial[K, V](p: K => Boolean, f: K => V): K ?=> V = { case x if p(x) => f(x) }
  def newCmp(difference: Long): Cmp                         = if (difference < 0) Cmp.LT else if (difference > 0) Cmp.GT else Cmp.EQ
  def newArray[A: CTag](size: Precise): Array[A]            = new Array[A](size.intSize)
  def newSize(n: Long): Precise                             = if (n < 0) Precise(0) else if (n > MaxInt) Precise(n) else Precise(n.toInt)
}
