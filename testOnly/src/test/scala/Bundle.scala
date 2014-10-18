package psp
package tests

import psp.std._
import org.scalacheck.Test
import scala.Console.{ println => _, _ }

trait Bundle {
  private var count = 0
  private var passed = 0

  def assert(body: => Boolean, msg: => Any): Unit = {
    count += 1
    Try(body).toOption match {
      case Some(true) => passed += 1
      case _          => System.err.println(s"Failed: $msg")
    }
  }
  def assert(body: => Boolean): Unit = {
    count += 1
    if (Try(body).toOption exists (x => x)) passed += 1
  }

  def finish(msg: String): Boolean = {
    val ok = count == passed
    val color = if (ok) GREEN else RED
    val str = color + "%3s/%-3s".format(passed, count) + RESET
    println(s"$str passed: $msg")
    ok
  }
  def finish(): Boolean = finish(this.shortClass stripSuffix "$")

  def run(): Boolean
}

/** Needed because scalacheck doesn't expose the label if you add
 *  labels with the |: operator.
 */
final class NamedProp(val label: String, p: Prop) {
  def prop = p :| label
}
object NamedProp {
  def apply(label: String, p: Prop): NamedProp                 = new NamedProp(label, p)
  implicit def liftSeqPair(x: (String, pSeq[Prop])): NamedProp = NamedProp(x._1, x._2 reducel (_ && _))
  implicit def liftPair(x: (String, Prop)): NamedProp          = NamedProp(x._1, x._2)
}

trait ScalacheckBundle extends Bundle {
  def bundle: String
  def props: Seq[NamedProp]

  def pass = GREEN + "pass" + RESET
  def fail = RED + "fail" + RESET
  def start = "+ " + BOLD + CYAN + bundle + RESET

  def pp(r: Result) = Pretty.pretty(r, Pretty.Params(0))
  def runOne(p: NamedProp): Boolean = Test.check(p.prop)(identity) match {
    case x if x.passed => andTrue(println("+ %s  %s".format(pass, p.label)))
    case r             => andFalse(println("- %s  %s\nFalsified after %s passed tests\n%s".format(p.label, fail, r.succeeded, pp(r))))
  }

  def run() = {
    println("\n" + start)
    props map runOne forall (x => x)
  }
  override def toString = bundle
}
