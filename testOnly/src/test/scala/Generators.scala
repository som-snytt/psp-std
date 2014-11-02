package psp
package tests

import org.scalacheck._, Gen._
import psp.std._, api._

package gen {
  object regex {
    def alternative: Gen[Regex]      = for ((r1, r2) <- zip(atom, atom)) yield r1 | r2
    def anchor(c: Char): Gen[String] = frequency(3 -> "", 1 -> c.toString)
    def apply(): Gen[Regex]          = for ((s, re, e) <- zip(anchor('^'), concatenate, anchor('$'))) yield re mapRegex (s + _ + e)
    def atom: Gen[Regex]             = oneOf(literal, range, quantified)
    def concatenate: Gen[Regex]      = choose(1, 3) flatMap (n => listOfN(n, simple) map (_ reduceLeft (_ | _)))
    def group: Gen[Regex]            = atom map (_.capturingGroup)
    def letter: Gen[Char]            = choose('a', 'f')
    def literal: Gen[Regex]          = choose(0, 5) flatMap (n => listOfN(n, letter)) map (_.mkString.r)
    def quantified: Gen[Regex]       = for ((re, c) <- zip(literal, oneOf("+?*".seq))) yield re mapRegex (_ + c)
    def range: Gen[Regex]            = for ((neg, s, e) <- zip(oneOf("", "^"), letter, letter) ; if (s <= e)) yield s"[$neg$s-$e]".r
    def simple: Gen[Regex]           = random(atom, group, alternative)
  }
}


package object gen {
  def random[A](xs: Direct[Gen[A]]): Gen[A] = indexRangeGen(xs.indices) flatMap xs.elemAt
  def random[A](xs: Gen[A]*): Gen[A]        = random(xs.m.pvec)

  def precise: Gen[Precise]              = chooseNum(1, MaxInt / 2) map (_.size)
  def bounded: Gen[Bounded]              = precise flatMap (lo => atomic map (hi => Size.bounded(lo, hi))) collect { case b: Bounded => b }
  def atomic: Gen[Atomic]                = frequency(10 -> precise, 1 -> 0.size, 1 -> Infinite)
  def size: Gen[Size]                    = oneOf(atomic, bounded)
  def long: Gen[Long]                    = Gen.choose(MinLong, MaxLong)
  def int: Gen[Int]                      = Gen.choose(MinInt, MaxInt)
  def posInt: Gen[Int]                   = Gen.choose(0, MaxInt)
  def uint: Gen[UInt]                    = int map UInt
  def letter: Gen[Char]                  = letterFrom("amnz09_")
  def word: Gen[String]                  = choose(1, 10) flatMap wordOfLength
  def line: Gen[String]                  = choose(3, 7) flatMap (n => listOfN(n, word)) map (_ mkString " ")
  def wordOfLength(n: Int): Gen[String]  = listOfN(n, letter) map (_.mkString)
  def lines(n: Int): Gen[Direct[String]] = choose(3, 7) flatMap (n => pvectorOfN(n, line))
  def letterFrom(s: String): Gen[Char]   = frequency(s.toCharArray map (c => 1 -> (c: Gen[Char])) seq: _*)

  def pvectorOf[A: Arb](g: Gen[A]): Gen[Direct[A]]          = containerOf[Direct, A](g)(?, _.toScalaTraversable)
  def pseqOf[A: Arb](g: Gen[A]): Gen[Each[A]]               = containerOf[Each, A](g)(?, _.toScalaTraversable)
  def pvectorOfN[A: Arb](n: Int, g: Gen[A]): Gen[Direct[A]] = containerOfN[Direct, A](n, g)(?, _.toScalaTraversable)
  def pseqOfN[A: Arb](n: Int, g: Gen[A]): Gen[Each[A]]      = containerOfN[Each, A](n, g)(?, _.toScalaTraversable)
  implicit class GenOps[A](gen: Gen[A]) {
    def collect[B](pf: A ?=> B): Gen[B]                                    = gen suchThat pf.isDefinedAt map pf.apply
    def collectN[B](n: Int)(pf: Each[A] ?=> B)(implicit z: Arb[A]): Gen[B] = pseqOfN(n, gen) collect pf
    def stream: Each[A]                                                    = Each continually gen.sample flatMap (_.pvec)
    def take(n: Int): Direct[A]                                            = stream take n pvec
  }
  def charToString(g: Gen[Char]): Gen[String]  = g map (_.to_s)
  def indexRangeGen(r: IndexRange): Gen[Index] = Gen.choose(r.start, r.endInclusive)

  implicit def chooseIndex: Choose[Index]  = Choose.xmap[Long, Index](_.index, _.indexValue)
  implicit def chooseSize: Choose[Precise] = Choose.xmap[Long, Precise](newSize, _.value)
  implicit def chooseNth: Choose[Nth]      = Choose.xmap[Long, Nth](_.nth, _.nthValue)
}
