package psp
package tests

import org.scalacheck._, Gen._
import psp.std._, api._, StdShow._

package gen {
  class RegexGenerator(val letter: Gen[Char]) {
    def alternative: Gen[Regex]       = (atom, atom) ^^ Regex.alt
    def anchor(c: Char): Gen[String]  = frequency(3 -> "", 1 -> c.toString)
    def apply(): Gen[Regex]           = (concatenate, anchor('^'), anchor('$')) map ((re, s, e) => re.surround(s, e))
    def atom: Gen[Regex]              = oneOf(literal, range, quantified)
    def concatenate: Gen[Regex]       = simple * (1 upTo 3) ^^ (_ reducel Regex.alt)
    def group: Gen[Regex]             = atom ^^ (_.capturingGroup)
    def letterPair: Gen[PairOf[Char]] = (letter, letter) filter (_ <= _)
    def literal: Gen[Regex]           = letter * (0 upTo 5) ^^ (_ mk_s "" r)
    def quantified: Gen[Regex]        = (literal zip oneOf("+?*".seq))(_ append _.to_s)
    def range: Gen[Regex]             = (oneOf("", "^"), letterPair) map { case (neg, (s, e)) => s"[$neg$s-$e]".r }
    def simple: Gen[Regex]            = oneOf(atom, group, alternative)
  }
  class TextGenerator(val letter: Gen[Char], charsInWord: Gen[Int], wordsInLine: Gen[Int]) {
    def word: Gen[String]                   = letter * charsInWord ^^ (_ mk_s "")
    def line: Gen[String]                   = word * wordsInLine ^^ (_ mk_s " ")
    def nLines(n: Int): Gen[Direct[String]] = line * n
  }

  object regex extends RegexGenerator(alphaLowerChar)
  object text extends TextGenerator(alphaNumChar, 1 upTo 8, 3 upTo 7)
}

package object gen {
  def chooseFrom[A](xs: Direct[Gen[A]]): Gen[A]       = indexFrom(xs.indices) >> xs.elemAt
  def chooseFrom[A](xs: Gen[A]*): Gen[A]              = chooseFrom(xs.m.pvec)
  def directOfN[A](n: Int, g: Gen[A]): Gen[Direct[A]] = containerOfN[Direct, A](n, g)(?, _.trav)
  def directOf[A](g: Gen[A]): Gen[Direct[A]]          = containerOf[Direct, A](g)(?, _.trav)
  def eachOfN[A](n: Int, g: Gen[A]): Gen[Each[A]]     = containerOfN[Each, A](n, g)(?, _.trav)
  def eachOf[A](g: Gen[A]): Gen[Each[A]]              = containerOf[Each, A](g)(?, _.trav)
  def pick[A](n: Int, xs: A*): Gen[Direct[A]]         = pick[A](n, xs.m.pvec)
  def pick[A](n: Int, xs: Direct[A]): Gen[Direct[A]]  = Gen.pick(n, xs.seq) map (_.m.pvec)

  def indexTo(max: Int): Gen[Index] = (0 upTo max) ^^ (n => Index(n))
  def index: Gen[Index]             = frequency(10 -> zeroPlusIndex, 1 -> NoIndex)
  def int: Gen[Int]                 = MinInt upTo MaxInt
  def long: Gen[Long]               = MinLong upTo MaxLong
  def posInt: Gen[Int]              = 1 upTo MaxInt
  def posLong: Gen[Long]            = 1L upTo MaxLong
  def uint: Gen[UInt]               = int ^^ UInt
  def zeroPlusIndex: Gen[Index]     = zeroPlusLong map Index
  def zeroPlusInt: Gen[Int]         = 0 upTo MaxInt
  def zeroPlusLong: Gen[Long]       = 0L upTo MaxLong

  def letterFrom(s: String): Gen[Char]                      = oneOf(s.seq)
  def indexFrom(r: IndexRange): Gen[Index]                  = Gen.choose(r.start, r.endInclusive)
  def indexRangeFrom(sMax: Int, eMax: Int): Gen[IndexRange] = (0 upTo sMax, 0 upTo eMax) ^^ indexRange
}
