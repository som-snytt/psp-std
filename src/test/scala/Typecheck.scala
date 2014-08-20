package psp
package tests

import psp.std._
import macros._
import scala.Console._
import Generated._

class Typecheck extends Bundle {
  def alpha(s: Char, e: Char): Seq[Char] = NumericRange.inclusive[Char](s, e, 1)
  def range(s: Int, e: Int): IndexRange  = Index(s) to Index(e)

  def atoz     = alpha('a', 'z')
  def alphabet = atoz.mkString

  def expect[A](msg: String, expected: A)(result: A)(implicit eqs: Eq[A], shows: Show[A]) = {
    def s1 = result.to_s
    def s2 = expected.to_s
    assert(expected === result, show"$msg == $s1 (expected: $s2)")
  }

  def check[A, B, C : Eq : Show](x: A, y: B, z: C)(f: (A, B) => String)(g: (A, B) => C): Unit = expect(f(x, y), z)(g(x, y))

  /** We'll say a line which begins with the shown comment is expected to type check.
   *  Will make this more robust. For now this makes it easy to put the expectation of
   *  success or failure next to the code in question.
   */
  def divide(what: String, xs: Vector[Typechecked]): Unit = divide(what, xs, xs count (_.code startsWith "/* ok */"))
  def divide(what: String, xs: Vector[Typechecked], expectedTypecheck: Int): Unit = {
    val (good, bad) = xs partition (_.typechecks)
    val passed = expectedTypecheck == good.size
    assert(passed, s"%s/%s expressions typechecked in %s, but expected %s/%s".format(good.size, xs.size, what, expectedTypecheck, xs.size))
  }

  def run(): Boolean = {
    check(alpha('a', 'g'), range(2, 4), alpha('c', 'e'))((x, y) => show"$x($y)")(_ apply _)
    check(alpha('a', 'g').mkString, range(2, 4), "cde")((x, y) => show"$x($y)")(_ apply _)
    check(alpha('a', 'g').toArray, range(2, 4), Array('c', 'd', 'e'))((x, y) => show"$x($y)")(_ apply _)

    // We don't want to protect scala library from itself so let's unmask augmentString etc.
    {
      import Predef._
      divide("scala-library", typecheckedLines(scalaLibraryCode), expectedTypecheck = 32)
    }

    divide("psp-std", typecheckedLines(pspCode), expectedTypecheck = 10)
    divide("psp-show", typecheckedLines(pspShowCode))

    // "%s/%s tests passed.".format(testsPassed, testsPassed + testsFailed)
    finish()
  }
}
