package psp
package tests

import psp.std._
import org.scalacheck.Test

// TODO - leverage now-available it-doesn't-typecheck test machinery.
class Nats extends Bundle {
  def ints = NatList(1, 2, 3, 4)
  def strs = NatList("a", "ab", "abc", "abcd")

  def run(): Boolean = {
    assert((ints zip ints map (_ + _)).sum == (ints.sum * 2))
    assert((ints zip ints zip ints map (_ + _ + _) sum) == ints.sum * 3)
    assert((ints zip strs zip ints map (_ + _.length + _) sum) == ints.sum * 3)
    finish()
  }
}

trait Bundle {
  private var count = 0
  private var passed = 0

  def assert(body: => Boolean, msg: => Any): Unit = {
    count += 1
    Try(body).toOption match {
      case Some(true) => passed += 1
      case _          => Console.err.println(s"Failed: $msg")
    }
  }
  def assert(body: => Boolean): Unit = {
    count += 1
    if (Try(body).toOption exists (x => x)) passed += 1
  }

  def finish(msg: String): Boolean = {
    val ok = count == passed
    val color = if (ok) Console.GREEN else Console.RED
    val str = color + "%3s/%-3s".format(passed, count) + Console.RESET
    println(s"$str passed: $msg")
    ok
  }
  def finish(): Boolean = finish(this.shortClass stripSuffix "$")

  def run(): Boolean
}

final case class NamedProp(label: String, prop: Prop)
object NamedProp {
  implicit def liftPair(x: (String, Prop)): NamedProp = NamedProp(x._1, x._2)
}

trait ScalacheckBundle extends Bundle {
  def bundle: String
  def props: Seq[NamedProp]

  def pass = Console.GREEN + "pass" + Console.RESET
  def fail = Console.RED + "fail" + Console.RESET

  def pp(r: Result) = Pretty.pretty(r, Pretty.Params(0))
  def runOne(p: NamedProp): Boolean = Test.check(p.prop)(identity) match {
    case x if p.label startsWith "---" => andTrue(println("+ %s".format(Console.CYAN + p.label.stripPrefix("---").trim + Console.RESET)))
    case x if x.passed                 => andTrue(println("+ %s  %s".format(pass, p.label)))
    case r                             => andFalse(println("- %s  %s\nFalsified after %s passed tests\n%s".format(p.label, fail, r.succeeded, pp(r))))
  }

  def run() = props map runOne forall (x => x)
}
