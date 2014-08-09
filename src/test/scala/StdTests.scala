package psp
package stdtests

import psp.std._
import macros._
import scala.Console._
import Generated._

class Tester {
  private var testsPassed = 0
  private var testsFailed = 0

  def alpha(s: Char, e: Char): Vector[Char] = s to e toVector
  def range(s: Int, e: Int): IndexRange     = Index(s) to Index(e)

  def atoz     = alpha('a', 'z')
  def alphabet = atoz.mkString
  def failed   = testsFailed > 0

  private def pass_s = GREEN + BOLD + "[pass]" + RESET
  private def fail_s = RED + BOLD + "[fail]" + RESET

  def echo(msg: Any)    = System.err.println(msg)
  def pass(msg: String) = try testsPassed += 1 finally echo(s"$pass_s $msg")
  def fail(msg: String) = try testsFailed += 1 finally echo(s"$fail_s $msg")

  def expect[A](msg: String, expected: A)(result: A)(implicit eqs: Eq[A], shows: Show[A]) = {
    def s1 = result.to_s
    def s2 = expected.to_s

    if (expected === result) pass(show"$msg == $s1")
    else fail(show"$msg == $s1 (expected: $s2)")
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
    if (passed)
      pass(s"%s/%s expressions typechecked in %s".format(good.size, xs.size, what))
    else {
      fail(s"%s/%s expressions typechecked in %s, but expected %s/%s".format(good.size, xs.size, what, expectedTypecheck, xs.size))
      good foreach (x => println("%-60s  %-20s  //  %s".format(x.code, x.tpe, x.tree)))
      println(bad mkString ("\n", "\n", "\n"))
    }
  }

  def run(): String = {
    check(alpha('a', 'g'), range(2, 4), alpha('c', 'e'))(_.to_s + "(" + _.to_s + ")")(_ apply _)
    check(alpha('a', 'g').mkString, range(2, 4), "cde")(_.to_s + "(" + _.to_s + ")")(_ apply _)
    check(alpha('a', 'g').toArray, range(2, 4), Array('c', 'd', 'e'))(_.to_s + "(" + _.to_s + ")")(_ apply _)

    divide("scala-library", typecheckedLines(scalaLibraryCode), expectedTypecheck = 32)
    divide("psp-std", typecheckedLines(pspCode), expectedTypecheck = 10)
    divide("psp-show", typecheckedLines(pspShowCode))

    "%s/%s tests passed.".format(testsPassed, testsPassed + testsFailed)
  }
}

object PspStdTests {
  def main(args: Array[String]): Unit = {
    val t = new Tester
    val msg = t.run
    if (t.failed)
      throw new Exception(msg)
    else
      println(msg)
  }
}
