package psp

import java.nio.{ file => jnf }
import jnf.{ attribute => jnfa }
import scala.{ collection => sc }
import sc.{ mutable => scm, immutable => sci }
import scala.sys.process.{ Process, ProcessBuilder }
import psp.std.api._
import psp.std.lowlevel._
import psp.std.StdShow._

package object std extends psp.std.StdPackage {
  type pSeq[+A]    = Foreach[A]
  type pVector[+A] = Direct[A]
  type pMap[K, V]  = PolicyMap[K, V]
  type pList[A]    = PolicyList[A]

  type inSet[A] = IntensionalSet[A]
  type exSet[A] = ExtensionalSet[A]
  // type inMap[A] = IntensionalMap[A]
  // type exMap[A] = ExtensionalMap[A]

  type DocSeq      = pSeq[Doc]

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
  final val Option  = psp.dmz.Option
  final val Seq     = psp.dmz.Seq
  final val Set     = psp.dmz.Set
  final val Some    = psp.dmz.Some
  final val System  = psp.dmz.System
  final val Try     = psp.dmz.Try
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

  final val ConstantTrue         = newPredicate[Any](_ => true)
  final val ConstantFalse        = newPredicate[Any](_ => false)
  final val CTag                 = scala.reflect.ClassTag
  final val EOL                  = sys.props.getOrElse("line.separator", "\n")
  final val NoFile: jFile        = jFile("")
  final val NoPath: Path         = path("")
  final val NoUri: jUri          = jUri("")
  final val NoFileTime: FileTime = jnfa.FileTime fromMillis MinLong
  final val NoIndex              = Index.undefined
  final val NoNth                = Nth.undefined

  type Walks[A0, Repr]                    = Walkable[Repr] { type A = A0 }
  type ForeachableType[A0, Repr, CC0[X]]  = Foreachable[Repr]  { type A = A0 ; type CC[B] = CC0[B] }
  type DirectAccessType[A0, Repr, CC0[X]] = DirectAccess[Repr] { type A = A0 ; type CC[B] = CC0[B] }

  def pClassOf[A: CTag](): PolicyClass      = new PolicyClass(classOf[A])
  def classOf[A: CTag](): Class[_ <: A]     = classTag[A].runtimeClass.castTo[Class[_ <: A]]
  def classLoaderOf[A: CTag](): ClassLoader = classOf[A].getClassLoader
  def nullLoader(): ClassLoader             = NullClassLoader
  def loaderOf[A: CTag] : ClassLoader       = noNull(classLoaderOf[A], nullLoader)
  def resource(name: String): Array[Byte]   = Try(noNull(currentThread.getContextClassLoader, nullLoader)) || loaderOf[this.type] fold (_ getResourceAsStream name slurp, _ => Array.empty)
  def resourceString(name: String): String  = utf8(resource(name)).to_s

  implicit def viewifyString(x: String): View[Char]          = x.m
  implicit def viewifyArray[A](x: Array[A]): View[A]         = x.m[DirectAccess] // must give this type argument explicitly.
  implicit def unViewifyString(x: View[Char]): String        = x.force[String]
  implicit def unViewifyArray[A: CTag](x: View[A]): Array[A] = x.force[Array[A]]

  // implicit def convertIntensional[K, V](x: Intensional[K, V]): K ?=> V = { case k if x contains k => x(k) }
  implicit def convertPolicySeq[A, B](xs: pSeq[A])(implicit conversion: A => B): pSeq[B] = xs map (x => conversion(x))
  implicit def scalaSeqToPSeq[A](x: scSeq[A]): pVector[A] = x.pvec

  def asserting[A](x: A)(assertion: => Boolean, msg: => String): A = x sideEffect assert(assertion, msg)

  def assert(assertion: Boolean): Unit                 = if (!assertion) assertionError("assertion failed")
  def assert(assertion: Boolean, msg: => Any): Unit    = if (!assertion) assertionError(s"assertion failed: $msg")
  def require(requirement: Boolean): Unit              = if (!requirement) illegalArgumentException("requirement failed")
  def require(requirement: Boolean, msg: => Any): Unit = if (!requirement) illegalArgumentException(s"requirement failed: $msg")
  def ??? : Nothing                                    = throw new scala.NotImplementedError
  def identity[A](x: A): A                             = x
  def implicitly[A](implicit x: A): A                  = x
  def locally[A](x: A): A                              = x

  def echoErr[A: TryShow](x: A): Unit                    = Console echoErr pp"$x"
  def println[A: TryShow](x: A): Unit                    = Console echoOut pp"$x"
  def printResult[A: TryShow](msg: String)(result: A): A = result doto (r => println(pp"$msg: $r"))
  def showResult[A: TryShow](msg: String)(result: A): A  = result doto (r => println(pp"$msg: $r"))

  def installedProviders: pVector[FileSystemProvider] = java.nio.file.spi.FileSystemProvider.installedProviders.m.pvec

  // Operations involving the filesystem.
  def path(s: String, ss: String*): Path                                     = ss.foldLeft(jnf.Paths get s)(_ resolve _)
  def newTempDir(prefix: String, attrs: AnyFileAttr*): Path                  = jnf.Files.createTempDirectory(prefix, attrs: _*)
  def newTempFile(prefix: String, suffix: String, attrs: AnyFileAttr*): Path = jnf.Files.createTempFile(prefix, suffix, attrs: _*)

  // Operations involving external processes.
  def newProcess(line: String): ProcessBuilder      = Process(line)
  def newProcess(args: Seq[String]): ProcessBuilder = Process(args)
  def executeLine(line: String): Int                = Process(line).!
  def execute(args: String*): Int                   = Process(args.toSeq).!

  def openSafari(path: Path): Unit = open.Safari(path)
  def openChrome(path: Path): Unit = open.`Google Chrome`(path)

  object open extends Dynamic {
    def applyDynamic(name: String)(args: TryShown*): String = Process(Seq("open", "-a", name) ++ args.map(_.to_s)).!!
  }

  def summonZero[A](implicit z: Zero[A]): Zero[A] = z

  def show[A: Show] : Show[A]        = ?

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

  // Operations involving classes, classpaths, and classloaders.
  def manifest[A: Manifest] : Manifest[A]             = implicitly[Manifest[A]]
  def classTag[T: CTag] : CTag[T]                     = implicitly[CTag[T]]

  // Operations involving Null, Nothing, and casts.
  def abortTrace(msg: String): Nothing     = new RuntimeException(msg) |> (ex => try throw ex finally ex.printStackTrace)
  def abort(msg: String): Nothing          = runtimeException(msg)
  def noNull[A](value: A, orElse: => A): A = if (value == null) orElse else value
  def nullAs[A] : A                        = asExpected[A](null)
  def asExpected[A](body: Any): A          = body.castTo[A]

  def intRange(start: Int, end: Int): ExclusiveIntRange = ExclusiveIntRange(start, end)
  def nthRange(start: Int, end: Int): ExclusiveIntRange = ExclusiveIntRange(start, end + 1)
  def indexRange(start: Int, end: Int): IndexRange      = IndexRange(start, end)

  def optImplicit[A](implicit value: A = null): Option[A] = if (value == null) None else Some(value)

  def ?[A](implicit value: A): A                = value
  def andFalse(x: Unit): Boolean                = false
  def andTrue(x: Unit): Boolean                 = true
  def direct[A](xs: A*): Direct[A]              = new Direct.FromScala(xs.toVector)
  def each[A](xs: sCollection[A]): Foreach[A]   = fromScala(xs)
  def nullStream(): InputStream                 = NullInputStream
  def offset(x: Int): Offset                    = Offset(x)
  def option[A](p: Boolean, x: => A): Option[A] = if (p) Some(x) else None
  def ordering[A: Order] : Ordering[A]          = ?[Order[A]].toScalaOrdering
  def regex(re: String): Regex                  = Regex(re)

  def fromScala[A](xs: sCollection[A]): Foreach[A] = xs match {
    case xs: scIndexedSeq[_] => new Direct.FromScala(xs.toIndexedSeq)
    case xs: sciLinearSeq[_] => new PolicyList.FromScala(xs)
    case xs: scSet[A]        => (new PolicySet.FromScala(xs.toSet[A])).m[Foreachable]
    case _                   => new Foreach.FromScala(xs)
  }
  def fromJava[A](xs: jIterable[A]): Foreach[A] = xs match {
    case xs: jList[_] => new Direct.FromJava[A](xs)
    case xs: jSet[_]  => (new PolicySet.FromJava[A](xs)).m[Foreachable]
    case xs           => new Foreach.FromJava[A](xs)
  }
  def fromElems[A](xs: A*): Foreach[A] = xs match {
    case xs: scmWrappedArray[A] => Direct fromArray xs.array
    case xs: sCollection[_]     => fromScala[A](xs)
    case _                      => fromScala(xs.toVector)
  }

  def convertSeq[A, B](xs: sciList[A])(implicit conversion: A => B): sciList[B]     = xs map conversion
  def convertSeq[A, B](xs: sciVector[A])(implicit conversion: A => B): sciVector[B] = xs map conversion
  def convertSeq[A, B](xs: scSeq[A])(implicit conversion: A => B): scSeq[B]         = xs map conversion
  def convertSeq[A, B](xs: pVector[A])(implicit conversion: A => B): pVector[B]     = xs map conversion
  def convertSeq[A, B](xs: Array[A])(implicit conversion: A => B): pVector[B]       = xs.pvec map conversion

  def mapBuilder[K, V](xs: (K, V)*): Builder[(K, V), scMap[K, V]] = sciMap.newBuilder[K, V] ++= xs
  def setBuilder[A](xs: A*): Builder[A, sciSet[A]]                = sciSet.newBuilder[A] ++= xs
  def listBuilder[A](xs: A*): Builder[A, sciList[A]]              = sciList.newBuilder[A] ++= xs
  def arrayBuilder[A: CTag](xs: A*): Builder[A, Array[A]]         = scala.Array.newBuilder[A] ++= xs
  def vectorBuilder[A](xs: A*): Builder[A, sciVector[A]]          = sciVector.newBuilder[A] ++= xs
  def mapToList[K, V](): scmMap[K, sciList[V]]                    = scmMap[K, sciList[V]]() withDefaultValue Nil

  // Java.
  def jMap[K, V](xs: (K, V)*): jMap[K, V]                = new jHashMap[K, V] doto (b => for ((k, v) <- xs) b.put(k, v))
  def jSet[A](xs: A*): jSet[A]                           = new jHashSet[A] doto (b => xs foreach b.add)
  def jList[A](xs: A*): jList[A]                         = java.util.Arrays.asList(xs: _* )
  def jFile(s: String): jFile                            = path(s).toFile
  def jUri(x: String): jUri                              = java.net.URI create x
  def jUrl(x: String): jUrl                              = jUri(x).toURL

  def concurrentMap[K, V](): ConcurrentMapWrapper[K, V]           = new ConcurrentMapWrapper[K, V](new ConcurrentHashMap[K, V], None)
  def concurrentMap[K, V](default: V): ConcurrentMapWrapper[K, V] = new ConcurrentMapWrapper[K, V](new ConcurrentHashMap[K, V], Some(default))

  // PolicyMap is our own creation since SortedMap is way overspecified
  // and LinkedHashMap is too slow and only comes in a mutable variety.
  def newMap[K : HashEq, V](kvs: (K, V)*): pMap[K, V]           = PolicyMap[K, V](PolicySet(kvs.m.toPolicyVector.map(_._1)), kvs.toMap)
  def newMap[K, V](keys: exSet[K], lookup: K ?=> V): pMap[K, V] = PolicyMap[K, V](keys, lookup)
  def newMap[K : HashEq, V](kvs: pSeq[(K, V)]): pMap[K, V]      = newMap(kvs.seq: _*)
  def newList[A](xs: A*): pList[A]                              = PolicyList(xs: _*)
  def newSet[A: HashEq](xs: A*): exSet[A]                       = PolicySet.elems[A](xs: _*)
  def newSeq[A](xs: A*): pSeq[A]                                = Direct[A](xs: _*)
  def newPredicate[A](f: Predicate[A]): Predicate[A]            = f
  def newPartial[K, V](p: K => Boolean, f: K => V): K ?=> V     = { case x if p(x) => f(x) }
  def newCmp(difference: Long): Cmp                             = if (difference < 0) Cmp.LT else if (difference > 0) Cmp.GT else Cmp.EQ

  def newArray[A: CTag](size: Precise): Array[A] = new Array[A](size.intSize)
  def newSize(n: Long): Precise = if (n < 0) Precise(0) else if (n > MaxInt) Precise(n) else Precise(n.toInt)
}
