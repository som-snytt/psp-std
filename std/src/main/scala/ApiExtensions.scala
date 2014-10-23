package psp
package std
package ops

import api._
import Doc._

trait DocStringOps extends Any {
  def self: String

  def <>(that: Doc): Doc  = self.asis <> that
  def <+>(that: Doc): Doc = self.asis <+> that
  def </>(that: Doc): Doc = self.asis </> that

  def doc: Doc           = asis
  def asis: Doc          = Text(self)
  def backticked: Doc    = Text("`" + self + "`")
  def %(args: Doc*): Doc = FormatString(self) % (args: _*)
}

class Renderer(indentSize: Int) {
  var indentLevel: Int = 0
  // var openGroups: pList[Doc]
  def sp: String = " " * (indentLevel * indentSize)

  private def indentedBy[A](n: Int)(body: => A): A = {
    indentLevel += n
    try body finally indentLevel -= n
  }

  def apply(doc: Doc): String = doc match {
    case WordBreak                       => " "
    case LineBreak                       => "\n"
    case Group(doc)                      => apply(doc)
    case Line(alt)                       => alt
    case Nest(doc, Indent(n))            => indentedBy(n)(apply(doc))
    case Text(value)                     => value
    case Shown(value, z)                 => z show value
    case Cat(left, right)                => apply(left) ~ apply(right)
    case Format(FormatString(fmt), args) => fmt.format(args.seq map apply: _*)
  }
}

class WeakApiViewOps[A](xs: View[A]) {
  def chainDescriptions: pVector[String] = xs.viewChain.reverse collect { case x: BaseView[_,_] => x } map (_.description)
}

class BaseViewOps[A, Repr](xs: BaseView[A, Repr]) {
  def tail: BaseView[A, Repr]                      = xs drop 1
  def init: BaseView[A, Repr]                      = xs dropRight 1
  def mapWithIndex[B](f: (A, Index) => B): View[B] = Foreach[B](mf => xs.foldl(0)((res, x) => try res + 1 finally mf(f(x, Index(res))))).m[Foreachable]
}

class DocOps(val lhs: Doc) extends AnyVal {
  def <>(rhs: String): Doc  = lhs <> rhs.asis
  def <+>(rhs: String): Doc = lhs <+> rhs.asis

  def <>(rhs: Doc): Doc   = Cat(lhs, rhs)
  def <+>(rhs: Doc): Doc  = lhs <> space <> rhs
  def </>(rhs: Doc): Doc  = lhs <> softline <> rhs
  def <@>(rhs: Doc): Doc  = lhs <> line <> rhs
  def <@@>(rhs: Doc): Doc = lhs <> linebreak <> rhs
  def <\>(rhs: Doc): Doc  = lhs <> softbreak <> rhs

  def render: String = new Renderer(2) apply lhs

  def to_str: String = lhs.render
  def length: Int    = to_str.length

  def last: Doc = lhs match {
    case Cat(left, right) => if (right.isEmpty) left.last else right.last
    case Group(x)         => x.last
    case _                => lhs
  }
  def first: Doc = lhs match {
    case Cat(left, right) => if (left.isEmpty) right.first else left.first
    case Group(x)         => x.first
    case _                => lhs
  }
  def isSpace: Boolean = lhs match {
    case WordBreak => true
    case Text(" ") => true
    case Group(x)  => x.isSpace
    case _         => false
  }
  def isEmpty: Boolean = lhs match {
    case Text("")  => true
    case Group(x)  => x.isEmpty
    case Cat(x, y) => x.isEmpty && y.isEmpty
    case _         => false
  }
  def endsSpace: Boolean = last.isSpace
  def endsLine: Boolean = last.isLine
  def isLine = lhs match {
    case Line(_) => true
    case _       => false
  }
  def isWhitespace: Boolean = isSpace || isLine
  def isNotWhitespace: Boolean = !isEmpty && !isWhitespace
  def indent(j: Int): Doc = Nest(linebreak <> lhs, Indent(j))
  def indent(): Doc       = indent(2)

  def surround(left: Doc, right: Doc): Doc       = left <> lhs <> right
  def surround(left: String, right: String): Doc = left.asis <> lhs <> right.asis

  def inParens: Doc       = surround(lparen, rparen)
  def inBraces: Doc       = surround(lbrace, rbrace)
  def inBrackets: Doc     = surround(lbracket, rbracket)
  def inDoubleQuotes: Doc = surround(dquote, dquote)
  def grouped: Doc        = Group(lhs)
}

trait DocSeqCommonOps extends Any {
  def docs: DocSeq

  import Doc._

  private def nonEmpties    = docs filterNot (_.isEmpty)
  private def isOnlyEmpties = nonEmpties.isEmpty

  // Empty if the seq is empty, otherwise apply the function.
  def opt(f: DocSeq => Doc): Doc = if (docs.isEmpty) empty else f(docs)

  def join(sep: Doc): Doc       = if (docs.isEmpty) empty else docs reducel (_ <> sep <> _)
  def joinSpaced(sep: Doc): Doc = if (docs.isEmpty) empty else docs reducel (_ <+> sep <+> _)

  def inBracesBlock: Doc = if (isOnlyEmpties) lbrace <> space <> rbrace else joinLines.indent() surround (lbrace, line <> rbrace)
  def inBracesList: Doc  = joinComma surround (lbrace <> space, space <> rbrace)
  def inParens: Doc      = joinComma.inParens
  def joinChars: Doc     = this join empty
  def joinComma: Doc     = nonEmpties join comma <> space
  def joinDotted: Doc    = this join dot
  def joinLines: Doc     = this join line
  def joinParents: Doc   = this joinSpaced "with".asis
  def joinWords: Doc     = nonEmpties join space
  def optBrackets: Doc   = if (isOnlyEmpties) empty else joinComma.inBrackets
}

final class DocSeqOps(val docs: DocSeq) extends AnyVal with DocSeqCommonOps
final class ShowableSeqOps[A: Show](xs: pSeq[A]) extends AnyRef with DocSeqCommonOps {
  def docs: DocSeq = xs map (_.doc)
}
final class IndexRangeOps(xs: IndexRange) {
  def *(n: Int): IndexRange = indexRange(xs.startInt * n, xs.endInt * n)
}

final class IntensionalSetOps[A](xs: inSet[A]) {
  def diff(that: inSet[A]): inSet[A]      = this filter that
  def filter(p: Predicate[A]): inSet[A]   = IntensionalSet.Filtered(xs, p)
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

  def filterNot(p: Predicate[A]): exSet[A] = ExtensionalSet.Filtered(xs, !p)
  def filter(p: Predicate[A]): exSet[A]    = ExtensionalSet.Filtered(xs, p)
  def union(that: exSet[A]): exSet[A]      = ExtensionalSet.Union(xs, that)
  def isSubsetOf(ys: inSet[A]): Boolean    = xs.m forall ys

  def mapContained(f: pSeq[A] => pSeq[A]): exSet[A] = PolicySet.direct(f(xs.contained))(xs.equiv, xs.hash)
}

trait HasPreciseSizeMethods extends Any {
  def size: Precise

  def longSize: Long      = size.value
  def intSize: Int        = longSize.safeToInt
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
  def size: Precise = x.size
}

final class PreciseOps(val size: Precise) extends AnyRef with HasPreciseSizeMethods {
  def get: Long   = longSize
  def getInt: Int = intSize
  def toDouble: Double = get.toDouble

  def + (n: Int): Precise = newSize(longSize + n)
  def - (n: Int): Precise = newSize(longSize - n)
  def * (n: Int): Precise = newSize(longSize * n)
  def / (n: Int): Precise = newSize(longSize / n)
  def % (n: Int): Precise = newSize(longSize % n)

  def /+ (n: Int): Precise = (this / n) + ( if ((this % n).isZero) 0 else 1 )

  def + (n: Precise): Precise = newSize(longSize + n.longSize)
  def - (n: Precise): Precise = newSize(longSize - n.longSize)
  def * (n: Precise): Precise = newSize(longSize * n.longSize)
  def / (n: Precise): Precise = newSize(longSize / n.longSize)
  def % (n: Precise): Precise = newSize(longSize % n.longSize)

  def min(that: Precise): Precise = newSize(longSize min that.longSize)
  def max(that: Precise): Precise = newSize(longSize max that.longSize)
  def increment: Precise          = newSize(longSize + 1L)
  def decrement: Precise          = newSize(longSize - 1L)

  def timesConst[A](elem: A): pSeq[A]   = Foreach constant elem take size
  def timesEval[A](body: => A): pSeq[A] = Foreach continually body take size

  def toIntRange                           = intRange(0, intSize)
  def padLeft(s: String, ch: Char): String = if (s.length >= longSize) s else (this - s.length timesConst ch mkString "") ~ s

  def leftFormatString  = if (size.isZero) "%s" else "%%-%ds" format intSize
  def rightFormatString = if (size.isZero) "%s" else "%%%ds" format intSize
  def leftFormat(arg: Doc): String  = leftFormatString format arg
  def rightFormat(arg: Doc): String = rightFormatString format arg

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

  def ++(ys: Direct[A]): Direct[A]                    = new Direct.Joined(xs, ys)
  def apply(i: Index): A                              = xs elemAt i
  def exclusiveEnd: Index                             = Index(length)
  def hasSameSize(that: HasSize): Boolean             = (xs: HasSize).size p_== that.size
  def head: A                                         = apply(0.index)
  def indicesAtWhich(p: Predicate[A]): pVector[Index] = xs.indices filter (i => p(apply(i)))
  def last: A                                         = apply(xs.size.lastIndex)
  def length: Int                                     = xs.size.intSize
  def nths: pVector[Nth]                              = xs mapIndices (_.toNth)
  def offsets: pVector[Offset]                        = xs mapIndices (_.toOffset)
  def runForeach(f: A => Unit): Unit                  = xs foreach f
  def takeRight(n: Precise): pVector[A]               = xs takeRight n

  def transformIndices(f: Index => Index): pVector[A] = new Direct.TransformIndices(xs, f)
  def reverse: Direct[A]  = xs match {
    case Direct.Reversed(xs) => xs
    case _                   => new Direct.Reversed(xs)
  }
}

final class ForeachOps[A](val xs: Foreach[A]) extends AnyVal with CommonOps[A, Foreach] {
  protected def underlying = xs
  def ++[A1 >: A](ys: Foreach[A1]): Foreach[A1] = Foreach.join(xs, ys)
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

  def +:(elem: A): CC[A]                                                            = rebuild(Foreach.join(fromElems(elem), xs))
  def :+(elem: A): CC[A]                                                            = rebuild(Foreach.join(xs, fromElems(elem)))
  def collect[B, That](pf: A ?=> B)(implicit z: Builds[B, That]): That              = z direct (f => xs foreach (x => if (pf isDefinedAt x) f(pf(x))))
  def distinct(implicit z: HashEq[A]): CC[A]                                        = rebuild(toPolicySet.contained)
  def drop(n: Precise): View[A]                                                     = xs.m drop n
  def filter(p: Predicate[A]): CC[A]                                                = withFilter(p)
  def filterNot(p: Predicate[A]): CC[A]                                             = withFilter(!p)
  def flatCollect[B, That](pf: A ?=> Foreach[B])(implicit z: Builds[B, That]): That = z direct (f => xs foreach (x => if (pf isDefinedAt x) pf(x) foreach f))
  def flatMap[B](g: A => Foreach[B]): CC[B]                                         = rebuild(Foreach[B](f => xs foreach (x => g(x) foreach f)))
  def flatten[B](implicit ev: A <:< Foreach[B]): CC[B]                              = rebuild(Foreach[B](f => xs foreach (x => ev(x) foreach f)))
  def map[B](g: A => B): CC[B]                                                      = rebuild(Foreach[B](f => xs foreach (x => f(g(x)))))
  def product(implicit z: Products[A]): A                                           = foldl(z.one)(z.product)
  def slice(range: IndexRange): View[A]                                             = this drop range.precedingSize take range.size
  def sum(implicit z: Sums[A]): A                                                   = foldl(z.zero)(z.sum)
  def take(n: Precise): View[A]                                                     = xs.m take n
  def withFilter(p: Predicate[A]): CC[A]                                            = rebuild(Foreach[A](f => xs foreach (x => if (p(x)) f(x))))
  def without(x: A)                                                                 = filterNot(_ id_== x)

  def reducel(f: (A, A) => A): A     = (xs take 1).force match { case PSeq(head) => (xs drop 1.size).foldl(head)(f) }
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
