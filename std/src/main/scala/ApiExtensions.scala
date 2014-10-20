package psp
package std
package ops

import api._
import java.{ lang => jl }

final class IndexRangeOps(xs: IndexRange) {
  def *(n: Int): IndexRange = indexRange(xs.startInt * n, xs.endInt * n)
}

final class IntensionalSetOps[A](xs: inSet[A]) {
  def diff(that: inSet[A]): inSet[A]      = this filter that
  def filter(p: A => Boolean): inSet[A]   = IntensionalSet.Filtered(xs, p)
  def union(that: inSet[A]): inSet[A]     = IntensionalSet.Union(xs, that)
  def intersect(that: inSet[A]): inSet[A] = IntensionalSet.Intersect(xs, that)
  def complement: inSet[A] = (xs: inSet[A]) match {
    case IntensionalSet.Complement(xs) => xs
    case _                             => IntensionalSet.Complement(xs)
  }
}
final class ExtensionalSetOps[A](xs: exSet[A]) {
  def intersect(that: exSet[A]): exSet[A] = ExtensionalSet.Intersect(xs, that)
  def diff(that: exSet[A]): exSet[A]      = ExtensionalSet.Diff(xs, that)
  def intersect(that: inSet[A]): exSet[A] = filter(that)
  def diff(that: inSet[A]): exSet[A]      = filterNot(that)

  def filterNot(p: A => Boolean): exSet[A] = ExtensionalSet.Filtered(xs, !p)
  def filter(p: A => Boolean): exSet[A]    = ExtensionalSet.Filtered(xs, p)
  def union(that: exSet[A]): exSet[A]      = ExtensionalSet.Union(xs, that)
  def isSubsetOf(ys: inSet[A]): Boolean    = xs.m forall ys

  def mapContained(f: pSeq[A] => pSeq[A]): exSet[A] = PolicySet.direct(f(xs.contained))(xs.equiv, xs.hash)
}

trait HasPreciseSizeMethods extends Any {
  def sizeInfo: PreciseSize

  def longSize: Long      = sizeInfo.value
  def intSize: Int        = longSize.safeToInt.zeroPlus
  def isZero: Boolean     = longSize == 0L
  def isPositive: Boolean = longSize > 0L
  def indices: IndexRange = indexRange(0, intSize)
  def lastIndex: Index    = Index(longSize - 1)  // effectively maps both undefined and zero to no index.
  def lastNth: Nth        = lastIndex.toNth

  def containsIndex(index: Index): Boolean            = indices contains index
  @inline def mapIndices[A](f: Index => A): Direct[A] = indices map f
  @inline def foreachIndex(f: Index => Unit): Unit    = indices foreach f
  @inline def foreachNth(f: Nth => Unit): Unit        = indices foreach (i => f(i.toNth))
}

final class HasPreciseSizeOps(val x: HasPreciseSize) extends HasPreciseSizeMethods {
  def sizeInfo = x.sizeInfo
}

final class PreciseSizeOps(val sizeInfo: PreciseSize) extends AnyRef with HasPreciseSizeMethods {
  def get: Long   = longSize
  def getInt: Int = intSize
  def toDouble: Double = get.toDouble

  def + (n: Int): PreciseSize = newSize(longSize + n)
  def - (n: Int): PreciseSize = newSize(longSize - n)
  def * (n: Int): PreciseSize = newSize(longSize * n)
  def / (n: Int): PreciseSize = newSize(longSize / n)
  def % (n: Int): PreciseSize = newSize(longSize % n)

  def /+ (n: Int): PreciseSize = (this / n) + ( if ((this % n).isZero) 0 else 1 )

  def + (n: PreciseSize): PreciseSize = newSize(longSize + n.longSize)
  def - (n: PreciseSize): PreciseSize = newSize(longSize - n.longSize)
  def * (n: PreciseSize): PreciseSize = newSize(longSize * n.longSize)
  def / (n: PreciseSize): PreciseSize = newSize(longSize / n.longSize)
  def % (n: PreciseSize): PreciseSize = newSize(longSize % n.longSize)

  def min(that: PreciseSize): PreciseSize = newSize(longSize min that.longSize)
  def max(that: PreciseSize): PreciseSize = newSize(longSize max that.longSize)
  def increment: PreciseSize              = newSize(longSize + 1L)
  def decrement: PreciseSize              = newSize(longSize - 1L)

  def timesConst[A](elem: A): pSeq[A]   = Foreach constant elem take sizeInfo
  def timesEval[A](body: => A): pSeq[A] = Foreach continually body take sizeInfo

  def toIntRange          = intRange(0, intSize)
  def leftFormat: String  = "%%-%ds" format intSize
  def rightFormat: String = "%%%ds" format intSize
  def padLeft(s: String, ch: Char): String = if (s.length >= longSize) s else (this - s.length timesConst ch mkString "") ~ s

  def containsRange(range: IndexRange): Boolean = range.endInt <= intSize

  override def toString = s"$longSize"
}

final class BooleanAlgebraOps[A](val algebra: BooleanAlgebra[A]) extends AnyVal {
  def map[B](f: B => A, g: A => B): BooleanAlgebra[B] = new Algebras.Mapped[A, B](algebra, f, g)
}
final class InputStreamOps(val in: InputStream) extends AnyVal {
  private def wrap[A](f: InputStream => A): A = buffered |> (in => f(in) sideEffect in.close)
  def buffered(): BufferedInputStream = in match {
    case buf: BufferedInputStream => buf
    case _                        => new BufferedInputStream(in)
  }
  def slurp(): Array[Byte] = slurp(-1)
  def slurp(len: Int): Array[Byte] = {
    val buf = arrayBuilder[Byte]()
    if (len >= 0) buf sizeHint len
    wrap { in =>
      var offset = 0
      val arr = new Array[Byte](InputStreamBufferSize)
      def loop() {
        if (offset < len || len < 0) {
          val read = in.read(arr, 0, InputStreamBufferSize)
          if (read >= 0) {
            offset += read
            buf ++= (arr take newSize(read)).seq
            loop()
          }
        }
      }
      loop()
      buf.result doto (xs => assert(len < 0 || xs.length == len, s"Could not read entire source ($offset of $len bytes)"))
    }
  }
}

final class DirectOps[A](val xs: Direct[A]) extends AnyVal with CommonOps[A, Direct] {
  protected def underlying = xs
  protected def rebuild[B](xs: pSeq[B]): pVector[B] = xs.pvec
  override def head: A = apply(0.index)

  // Foreach from 0
  // 1 to 100 splitSeq (_ splitAt 3.index) take 3 foreach println
  def apply(i: Index): A                              = xs elemAt i
  def exclusiveEnd: Index                             = Index(length)
  def isNoLargerThan(that: SizeInfo): Boolean         = (xs: HasSizeInfo).sizeInfo p_<= that
  def isNoLargerThan(that: HasSizeInfo): Boolean      = isNoLargerThan(that.sizeInfo)
  def hasSameSize(that: HasSizeInfo): Boolean         = (xs: HasSizeInfo).sizeInfo p_== that.sizeInfo
  def indicesAtWhich(p: Predicate[A]): pVector[Index] = xs.indices filter (i => p(apply(i)))
  def init: pVector[A]                                = xs.m dropRight 1.size force
  def tail: pVector[A]                                = xs drop 1.size force
  def last: A                                         = apply(xs.sizeInfo.lastIndex)
  def length: Int                                     = xs.sizeInfo.intSize
  def nths: pVector[Nth]                              = xs mapIndices (_.toNth)
  def offsets: pVector[Offset]                        = xs mapIndices (_.toOffset)
  def runForeach(f: A => Unit): Unit                  = xs foreach f
  def takeRight(n: PreciseSize): pVector[A]           = xs takeRight n

  def transformIndices(f: Index => Index): pVector[A] = new Direct.TransformIndices(xs, f)
  def reverse: Direct[A]  = xs match {
    case Direct.Reversed(xs) => xs
    case _                   => new Direct.Reversed(xs)
  }
}

final class ForeachOps[A](val xs: Foreach[A]) extends AnyVal with CommonOps[A, Foreach] {
  protected def underlying = xs
  // def +: (elem: A): Foreach[A] = Foreach.join(direct(elem), xs)
  // def :+ (elem: A): Foreach[A] = Foreach.join(xs, direct(elem))
  def toRefs: pSeq[AnyRef] = xs map (_.toRef)
  def runForeach(f: A => Unit): Unit = xs foreach f
  protected def rebuild[B](xs: Foreach[B]): Foreach[B] = xs
}

trait CommonOps[A, CC[X] <: Foreach[X]] extends Any with CombinedOps[A] with FrontSliceable[View[A]] {
  def xs: CC[A]
  def build(implicit z: Builds[A, CC[A]]): CC[A] = force[CC[A]]
  protected def rebuild[B](xs: Foreach[B]): CC[B]

  def take(n: PreciseSize): View[A]     = xs.m take n
  def drop(n: PreciseSize): View[A]     = xs.m drop n
  def slice(range: IndexRange): View[A] = this drop range.precedingSize take range.sizeInfo

  def distinct(implicit z: HashEq[A]): CC[A]                                        = rebuild(toPolicySet.contained)
  def flatten[B](implicit ev: A <:< Foreach[B]): CC[B]                              = rebuild(Foreach[B](f => xs foreach (x => ev(x) foreach f)))
  def map[B](g: A => B): CC[B]                                                      = rebuild(Foreach[B](f => xs foreach (x => f(g(x)))))
  def flatMap[B](g: A => Foreach[B]): CC[B]                                         = rebuild(Foreach[B](f => xs foreach (x => g(x) foreach f)))
  def withFilter(p: Predicate[A]): CC[A]                                            = rebuild(Foreach[A](f => xs foreach (x => if (p(x)) f(x))))
  def collect[B, That](pf: A ?=> B)(implicit z: Builds[B, That]): That              = z(f => xs foreach (x => if (pf isDefinedAt x) f(pf(x))))
  def flatCollect[B, That](pf: A ?=> Foreach[B])(implicit z: Builds[B, That]): That = z(f => xs foreach (x => if (pf isDefinedAt x) pf(x) foreach f))

  def filter(p: Predicate[A]): CC[A]       = withFilter(p)
  def filterNot(p: Predicate[A]): CC[A]    = withFilter(!p)
  def sum(implicit num: Numeric[A]): A     = foldl(num.zero)(num.plus)
  def product(implicit num: Numeric[A]): A = foldl(num.one)(num.times)
  def ++[A1 >: A](ys: Foreach[A1]): CC[A1] = rebuild(Foreach.join(xs, ys))

  def +:(elem: A): CC[A] = rebuild(Foreach.join(fromElems(elem), xs))
  def :+(elem: A): CC[A] = rebuild(Foreach.join(xs, fromElems(elem)))

  def without(x: A) = filterNot(_ id_== x)

  def head: A = {
    take(1.size).force.foreach(x => return x)
    abort("empty.head")
  }

  def reducel(f: (A, A) => A): A     = drop(1.size).foldl(head)(f)
  def max(implicit ord: Order[A]): A = reducel(_ max2 _)
  def min(implicit ord: Order[A]): A = reducel(_ min2 _)
}

final class StdOptOps[A](val x: Opt[A]) extends AnyVal {
  def fold[B](none: => B)(f: A => B): B = if (x.isEmpty) none else f(x.get)
  def |[A1 >: A](alt: => A1): A1        = if (x.isEmpty) alt else x.get
}

final class OrderBy[A]   { def apply[B](f: A => B)(implicit z: Order[B]): Order[A]   = Order[A]((x, y) => z.compare(f(x), f(y))) }
final class EqBy[A]      { def apply[B](f: A => B)(implicit z: Eq[B]): Eq[A]         = Eq[A]((x, y) => z.equiv(f(x), f(y)))      }
final class ShowBy[A]    { def apply[B](f: A => B)(implicit z: Show[B]): Show[A]     = Show[A](x => z show f(x))                 }
final class HashBy[A]    { def apply[B](f: A => B)(implicit z: Hash[B]): Hash[A]     = Hash[A](x => z hash f(x))                 }
final class HashEqBy[A]  { def apply[B](f: A => B)(implicit z: HashEq[B]): HashEq[A] = HashEq[A]((x, y) => z.equiv(f(x), f(y)), x => z hash f(x)) }
