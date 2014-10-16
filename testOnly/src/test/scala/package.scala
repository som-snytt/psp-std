package psp

import org.scalacheck._, Gen._, Prop._
import psp.std._, api._
import SizeInfo._
import StdShow._

package object tests {
  lazy val isTestDebug = sys.props contains "psp.test.debug"

  type BinOp[A]           = (A, A) => A
  type Relation[A]        = (A, A) => Boolean
  type Forall1[-A]        = A => Boolean
  type Forall2[-A]        = (A, A) => Boolean
  type Forall3[-A]        = (A, A, A) => Boolean
  type Prop               = org.scalacheck.Prop
  val Prop                = org.scalacheck.Prop
  val Pretty              = org.scalacheck.util.Pretty
  type Result             = org.scalacheck.Test.Result
  type Failed             = org.scalacheck.Test.Failed
  type Buildable[A, C[X]] = org.scalacheck.util.Buildable[A, C]

  def assignable(storage: jClass, value: jClass): NamedProp =
    show"$storage isAssignablefrom $value" -> Prop(storage isAssignableFrom value)

  def allAssignable(storage: jClass, values: Seq[jClass]): NamedProp =
    show"$storage isAssignableFrom [${values.size} values]" -> (values map (v => Prop(storage isAssignableFrom v)))

  def checkResult[A, T: CTag](result: A): NamedProp    = assignable(classOf[T], result.getClass)
  def checkResults[A, T: CTag](results: A*): NamedProp = allAssignable(classOf[T], results map (_.getClass))

  implicit def buildsToBuildable[A, CC[X]](implicit z: Builds[A, CC[A]]): Buildable[A, CC] =
    new Buildable[A, CC] { def builder = Vector.newBuilder[A] mapResult (r => z build Foreach.elems(r: _*)) }

  implicit class GenOps[A](gen: Gen[A]) {
    def collect[B](pf: A ?=> B): Gen[B]                   = gen suchThat pf.isDefinedAt map pf.apply
    def collectN[B](n: Int)(pf: sciList[A] ?=> B): Gen[B] = containerOfN[sciList, A](n, gen) collect pf
    def stream: scIterator[A]                             = scIterator continually gen.sample flatMap (x => x)
    def take(n: Int): pVector[A]                          = (stream take n).toSeq.pvec
    def next(): A                                         = stream.next
  }
  implicit class PropOps(p: Prop) {
    def unary_! : Prop = p map (r => !r)
  }
  implicit class PropResultOps(r: Prop.Result) {
    def unary_! : Prop.Result = r.copy(status = !r.status)
  }
  implicit class StatusOps(s: Prop.Status) {
    def unary_! : Prop.Status = s match {
      case Proof => False
      case True  => False
      case False => True
      case x     => x
    }
  }
  implicit class LiftConverter[A](gen: Gen[A]) {
    def to[B](implicit f: A => B): Gen[B] = gen map f
  }

  implicit def charToString(g: Gen[Char]): Gen[String]  = g map ("" + _)
  implicit def chooseIndex: Choose[Index]               = Choose.xmap[Long, Index](_.index, _.indexValue)
  implicit def chooseSize: Choose[PreciseSize]          = Choose.xmap[Long, PreciseSize](n => PreciseSize(n), _.value)
  implicit def chooseNth: Choose[Nth]                   = Choose.xmap[Long, Nth](_.nth, _.nthValue)
  implicit def indexRangeGen(r: IndexRange): Gen[Index] = Gen.choose(r.start, r.endInclusive)

  def randomGen[A](xs: pVector[Gen[A]]): Gen[A] = indexRangeGen(xs.indices) flatMap xs.elemAt
  def randomGen[A](xs: Gen[A]*): Gen[A]         = randomGen(xs.seq.pvec)

  def genPrecise: Gen[PreciseSize] = chooseNum(1, MaxInt / 2) map (s => PreciseSize(s))
  def genBounded: Gen[Bounded]     = genPrecise flatMap (lo => genAtomic map (hi => bounded(lo, hi))) collect { case b: Bounded => b }
  def genAtomic: Gen[Atomic]       = frequency(10 -> genPrecise, 1 -> Empty, 1 -> Infinite)
  def genSizeInfo: Gen[SizeInfo]   = oneOf(genAtomic, genBounded)
  def genLong: Gen[Long]           = Gen.choose(MinLong, MaxLong)
  def genInt: Gen[Int]             = Gen.choose(MinInt, MaxInt)
  def genPosInt: Gen[Int]          = Gen.choose(0, MaxInt)
  def genUInt: Gen[UInt]           = genInt map UInt

  object genregex {
    def letter: Gen[Char]            = choose[Char]('a', 'f')
    def literal: Gen[Regex]          = choose(0, 5) flatMap (n => listOfN(n, letter)) map (_.mkString.r)
    def range: Gen[Regex]            = for ((neg, s, e) <- zip(oneOf("", "^"), letter, letter) ; if (s <= e)) yield s"[$neg$s-$e]".r
    def quantified: Gen[Regex]       = for ((re, c) <- zip(literal, oneOf("+?*".seq))) yield re mapRegex (_ + c)
    def atom: Gen[Regex]             = oneOf(literal, range, quantified)
    def group: Gen[Regex]            = atom map (_.capturingGroup)
    def alternative: Gen[Regex]      = for ((r1, r2) <- zip(atom, atom)) yield r1 | r2
    def simple: Gen[Regex]           = randomGen(atom, group, alternative)
    def concatenate: Gen[Regex]      = choose(1, 3) flatMap (n => listOfN(n, simple) map (_ reduceLeft (_ | _)))
    def anchor(c: Char): Gen[String] = frequency(3 -> "", 1 -> c.toString)
    def regex: Gen[Regex]            = for ((s, re, e) <- zip(anchor('^'), concatenate, anchor('$'))) yield re mapRegex (s + _ + e)
  }

  def genRegex: Gen[Regex] = genregex.regex
}
