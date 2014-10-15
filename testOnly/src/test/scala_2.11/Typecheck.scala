package psp
package tests

import psp.std._, api._, macros._
import Generated._
import StdEq._

class Typecheck extends Bundle {
  def alpha(s: Char, e: Char): sciIndexedSeq[Char] = sciNumericRange.inclusive[Char](s, e, 1)
  def atoz                                         = alpha('a', 'z')
  def alphabet                                     = atoz.mkString

  def expect[A](msg: String, expected: A)(result: A)(implicit eqs: Eq[A], shows: TryShow[A]) = {
    def s1 = result.to_s
    def s2 = expected.to_s
    assert(expected === result, pp"$msg == $s1 (expected: $s2)")
  }

  def check[A, B, C : Eq : TryShow](x: A, y: B, z: C)(f: (A, B) => String)(g: (A, B) => C): Unit = expect(f(x, y), z)(g(x, y))

  /** We'll say a line which begins with the shown comment is expected to type check.
   *  Will make this more robust. For now this makes it easy to put the expectation of
   *  success or failure next to the code in question.
   */
  def divide(what: String, xs: sciVector[Typechecked]): Unit = divide(what, xs, xs count (_.code startsWith "/* ok */"))
  def divide(what: String, xs: sciVector[Typechecked], expectedTypecheck: Int): Unit = {
    val (good, bad) = xs partition (_.typechecks)
    val passed = expectedTypecheck == good.size
    assert(passed, s"%s/%s expressions typechecked in %s, but expected %s/%s\ngood:\n%s\nbad:\n%s".format(
      good.size, xs.size, what, expectedTypecheck, xs.size, good mkString "\n", bad mkString "\n"))
  }

  def run(): Boolean = {
    check(alpha('a', 'g'), indexRange(2, 5), alpha('c', 'e'))((x, y) => pp"$x($y)")(_.m slice _ force)
    check(alpha('a', 'g').mkString, indexRange(2, 5), "cde")((x, y) => pp"$x($y)")(_.m slice _ force)
    check(alpha('a', 'g').toArray, indexRange(2, 5), Array('c', 'd', 'e'))((x, y) => pp"$x($y)")(_.m slice _ force)

    // We don't want to protect scala library from itself so let's unmask augmentString etc.
    {
      val pspAugmentString = null
      // val opsDirectString = null
      import scala.Predef._
      divide("scala-library", typecheckedLines(scalaLibraryCode), expectedTypecheck = 32)
    }

    divide("psp-std", typecheckedLines(pspCode), expectedTypecheck = 10)
    divide("psp-show", typecheckedLines(pspShowCode))

    // "%s/%s tests passed.".format(testsPassed, testsPassed + testsFailed)
    finish()
  }
}
