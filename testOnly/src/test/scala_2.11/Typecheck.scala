package psp
package tests

import psp.std._, api._, macros._
import Expressions._, StdShow._

class Typecheck extends ScalacheckBundle {
  def bundle = "Verifying Expected Failures"
  // We don't want to protect scala library from itself so let's unmask augmentString etc.
  def checkScala() = {
    val opsDirectString = null
    val directStringIs = null
    import scala.Predef._
    divide("scala-library", typecheckedLines(scalaLibraryCode), expectedTypecheck = 24)
  }

  /** We'll say a line which begins with the shown comment is expected to type check.
   *  Will make this more robust. For now this makes it easy to put the expectation of
   *  success or failure next to the code in question.
   */
  def divide(what: String, xs: sciVector[Typechecked]): NamedProp = divide(what, xs, xs count (_.code startsWith "/* ok */"))

  def divide(what: String, xs: sciVector[Typechecked], expectedTypecheck: Int): NamedProp = {
    val (good, bad) = xs partition (_.typechecks)
    NamedProp(
      s"$expectedTypecheck/${xs.size} expressions from $what should typecheck",
      (Prop(expectedTypecheck == good.size) :|
        ("good:\n  " + (good mkString "\n  ") + "\n\nbad:\n  " + (bad mkString "\n  ") + "\n")
      )
    )
  }

  def props = Direct[NamedProp](
    divide("psp-show", typecheckedLines(pspShowCode)),
    divide("psp-by-equals", typecheckedLines(policyByEquals), expectedTypecheck = 12),
    divide("psp-by-ref", typecheckedLines(policyByRef), expectedTypecheck = 0),
    divide("psp-straight", typecheckedLines(scalaLibraryCode), expectedTypecheck = 14),
    checkScala()
  )
}
