package psp
package tests

import org.scalacheck._, Gen._
import psp.std._, api._, StdShow._

final case class SizeRange(min: Precise, max: Precise) {
  def chooseInt = choose(min.safeInt, max.safeInt)
}
object SizeRange {
  def apply(min: Int, max: Int): SizeRange = new SizeRange(min.size, max.size)
}

package gen {
  class RegexGenerator(alphabet: Gen[Char]) {
    def alternative: Gen[Regex]      = for ((r1, r2) <- zip(atom, atom)) yield r1 | r2
    def anchor(c: Char): Gen[String] = frequency(3 -> "", 1 -> c.toString)
    def apply(): Gen[Regex]          = for ((s, re, e) <- zip(anchor('^'), concatenate, anchor('$'))) yield re mapRegex (s + _ + e)
    def atom: Gen[Regex]             = oneOf(literal, range, quantified)
    def concatenate: Gen[Regex]      = choose(1, 3) >> (n => directOfN(n, simple) map (_ reducel (_ | _)))
    def group: Gen[Regex]            = atom map (_.capturingGroup)
    def letter: Gen[Char]            = choose('a', 'f')
    def literal: Gen[Regex]          = choose(0, 5) >> (letter * _) ^^ (_ mk_s "" r)
    def quantified: Gen[Regex]       = for ((re, c) <- zip(literal, oneOf("+?*".seq))) yield re mapRegex (_ + c)
    def range: Gen[Regex]            = for ((neg, s, e) <- zip(oneOf("", "^"), letter, letter) ; if (s <= e)) yield s"[$neg$s-$e]".r
    def simple: Gen[Regex]           = chooseFrom(atom, group, alternative)
  }
  class TextGenerator(val letter: Gen[Char], charsInWord: SizeRange, wordsInLine: SizeRange) {
    def word: Gen[String]                   = charsInWord.chooseInt >> nLetters
    def line: Gen[String]                   = wordsInLine.chooseInt >> (n => directOfN(n, word) map (_ mk_s " "))
    def nLines(n: Int): Gen[Direct[String]] = directOfN(n, line)
    def nLetters(n: Int): Gen[String]       = directOfN(n, letter) map (_ mk_s "")
  }

  object regex extends RegexGenerator(alphaLowerChar)
  object text extends TextGenerator(alphaNumChar, SizeRange(1, 8), SizeRange(3, 7))
}

package object gen {
  def eachOfN[A: Arb](n: Int, g: Gen[A]): Gen[Each[A]] = containerOfN[Each, A](n, g)(?, _.toScalaTraversable)
  def eachOf[A: Arb](g: Gen[A]): Gen[Each[A]]          = containerOf[Each, A](g)(?, _.toScalaTraversable)
  def directOfN[A](n: Int, g: Gen[A]): Gen[Direct[A]]  = containerOfN[Direct, A](n, g)(?, _.toScalaTraversable)
  def directOf[A](g: Gen[A]): Gen[Direct[A]]           = containerOf[Direct, A](g)(?, _.toScalaTraversable)
  def chooseFrom[A](xs: Direct[Gen[A]]): Gen[A]        = indexFrom(xs.indices) >> xs.elemAt
  def chooseFrom[A](xs: Gen[A]*): Gen[A]               = chooseFrom(xs.m.pvec)

  def int: Gen[Int]             = Gen.choose(MinInt, MaxInt)
  def long: Gen[Long]           = Gen.choose(MinLong, MaxLong)
  def zeroPlusInt: Gen[Int]     = Gen.choose(0, MaxInt)
  def zeroPlusLong: Gen[Long]   = Gen.choose(0, MaxLong)
  def posInt: Gen[Int]          = Gen.choose(1, MaxInt)
  def posLong: Gen[Long]        = Gen.choose(1, MaxLong)
  def zeroPlusIndex: Gen[Index] = zeroPlusLong map Index
  def index: Gen[Index]         = frequency(10 -> zeroPlusIndex, 1 -> NoIndex)
  def uint: Gen[UInt]           = int map UInt

  def letterFrom(s: String): Gen[Char]     = frequency(s.toCharArray map (c => 1 -> (c: Gen[Char])) seq: _*)
  def indexFrom(r: IndexRange): Gen[Index] = Gen.choose(r.start, r.endInclusive)
}
