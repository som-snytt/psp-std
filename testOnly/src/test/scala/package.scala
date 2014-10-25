package psp

import org.scalacheck._, Gen._, Prop._
import psp.std._, api._
import Size._
import StdShow._

package object tests {
  lazy val isTestDebug = sys.props contains "psp.test.debug"

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

  def arb[A](implicit z: Arbitrary[A]): Arbitrary[A] = z

  implicit def arbitraryIntensionalSet[A : Arbitrary : HashEq] : Arbitrary[inSet[A]] = arb[sciSet[A]] map (_.m.toPolicySet)
  implicit def arbitraryPint: Arbitrary[Pint]                                        = Arbitrary(Gen.choose(MinInt, MaxInt) map (x => Pint(x)))

  def showsAs[A: Show](expected: String, x: A): NamedProp         = expected -> (expected =? show"$x")
  def seqShows[A: Show](expected: String, xs: pSeq[A]): NamedProp = expected -> (expected =? (xs mk_s ", "))

  def expectType(expected: jClass, found: jClass): NamedProp        = fshow"$expected%15s  >:>  $found%s" -> Prop(expected isAssignableFrom found)
  def expectTypes(expected: jClass, found: pSeq[jClass]): NamedProp = fshow"$expected%15s  >:>  $found%s" -> found.map(c => Prop(expected isAssignableFrom c))

  def expectType[A: CTag](result: A): NamedProp                     = expectType(classOf[A], result.getClass)
  def expectTypes[A: CTag](results: A*): NamedProp                  = expectTypes(classOf[A], fromScala(results) map (_.getClass))

  implicit def buildsToBuildable[A, CC[X]](implicit z: Builds[A, CC[A]]): Buildable[A, CC] =
    new Buildable[A, CC] { def builder: scmBuilder[A, CC[A]] = z.scalaBuilder }

  def pvectorOf[A: Arbitrary](g: Gen[A]): Gen[pVector[A]]          = containerOf[pVector, A](g)(?, _.toScalaTraversable)
  def pseqOf[A: Arbitrary](g: Gen[A]): Gen[pSeq[A]]                = containerOf[pSeq, A](g)(?, _.toScalaTraversable)
  def pvectorOfN[A: Arbitrary](n: Int, g: Gen[A]): Gen[pVector[A]] = containerOfN[pVector, A](n, g)(?, _.toScalaTraversable)
  def pseqOfN[A: Arbitrary](n: Int, g: Gen[A]): Gen[pSeq[A]]       = containerOfN[pSeq, A](n, g)(?, _.toScalaTraversable)

  implicit class ArbitraryOps[A](x: Arbitrary[A]) {
    def map[B](f: A => B): Arbitrary[B]       = Arbitrary(x.arbitrary map f)
    def filter(p: Predicate[A]): Arbitrary[A] = Arbitrary(x.arbitrary filter p)
  }
  implicit class GenOps[A](gen: Gen[A]) {
    def collect[B](pf: A ?=> B): Gen[B]                                          = gen suchThat pf.isDefinedAt map pf.apply
    def collectN[B](n: Int)(pf: pSeq[A] ?=> B)(implicit z: Arbitrary[A]): Gen[B] = pseqOfN(n, gen) collect pf
    def stream: pSeq[A]                                                          = Foreach continually gen.sample flatMap (_.pvec)
    def take(n: Int): pVector[A]                                                 = stream take n pvec
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

  def charToString(g: Gen[Char]): Gen[String]  = g map (_.to_s)
  def indexRangeGen(r: IndexRange): Gen[Index] = Gen.choose(r.start, r.endInclusive)

  implicit def chooseIndex: Choose[Index]  = Choose.xmap[Long, Index](_.index, _.indexValue)
  implicit def chooseSize: Choose[Precise] = Choose.xmap[Long, Precise](newSize, _.value)
  implicit def chooseNth: Choose[Nth]      = Choose.xmap[Long, Nth](_.nth, _.nthValue)

  def randomGen[A](xs: pVector[Gen[A]]): Gen[A] = indexRangeGen(xs.indices) flatMap xs.elemAt
  def randomGen[A](xs: Gen[A]*): Gen[A]         = randomGen(xs.m.pvec)

  def genPrecise: Gen[Precise]               = chooseNum(1, MaxInt / 2) map (x => newSize(x))
  def genBounded: Gen[Bounded]               = genPrecise flatMap (lo => genAtomic map (hi => bounded(lo, hi))) collect { case b: Bounded => b }
  def genAtomic: Gen[Atomic]                 = frequency(10 -> genPrecise, 1 -> Empty, 1 -> Infinite)
  def genSize: Gen[Size]                     = oneOf(genAtomic, genBounded)
  def genLong: Gen[Long]                     = Gen.choose(MinLong, MaxLong)
  def genInt: Gen[Int]                       = Gen.choose(MinInt, MaxInt)
  def genPosInt: Gen[Int]                    = Gen.choose(0, MaxInt)
  def genUInt: Gen[UInt]                     = genInt map UInt
  def genLetter: Gen[Char]                   = letterFrom("amnz09_")
  def genWord: Gen[String]                   = choose(1, 10) flatMap genWordOfLength
  def genLine: Gen[String]                   = choose(3, 7) flatMap (n => listOfN(n, genWord)) map (_ mkString " ")
  def genWordOfLength(n: Int): Gen[String]   = listOfN(n, genLetter) map (_.mkString)
  def genLines(n: Int): Gen[pVector[String]] = choose(3, 7) flatMap (n => pvectorOfN(n, genLine))
  def letterFrom(s: String): Gen[Char]       = frequency(s.toCharArray map (c => 1 -> (c: Gen[Char])) seq: _*)

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

package tests {
  final case class Pint(x: Int) { override def toString = s"$x" }
}
