package psp
package tests

import psp.std._, api._, macros._
import Generated._, StdShow._

class Typecheck extends ScalacheckBundle {
  def bundle = "Verifying Expected Failures"
  // We don't want to protect scala library from itself so let's unmask augmentString etc.
  def checkScala() = {
    val opsDirectString = null
    val directStringIs = null
    import scala.Predef._
    divide("scala-library", typecheckedLines(scalaLibraryCode), expectedTypecheck = 32)
  }

  /** We'll say a line which begins with the shown comment is expected to type check.
   *  Will make this more robust. For now this makes it easy to put the expectation of
   *  success or failure next to the code in question.
   */
  def divide(what: String, xs: sciVector[Typechecked]): NamedProp = divide(what, xs, xs count (_.code startsWith "/* ok */"))

  def divide(what: String, xs: sciVector[Typechecked], expectedTypecheck: Int): NamedProp = {
    val (good, bad) = xs partition (_.typechecks)
    s"$expectedTypecheck/${xs.size} expressions from $what should typecheck" -> (Prop(expectedTypecheck == good.size) :| pp"good:\n$good\nbad:\n$bad")
  }

  def props = sciSeq[NamedProp](
    divide("psp-std", typecheckedLines(pspCode), expectedTypecheck = 10),
    divide("psp-show", typecheckedLines(pspShowCode)),
    checkScala()
  )
}
