package psp
package tests

import psp.std._, psp.core._

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

abstract class Bundle {
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

  def finish(msg: String): Boolean = try count == passed finally println(f"$passed%3s/$count%-3s passed: $msg")
  def finish(): Boolean            = finish(this.shortClass stripSuffix "$")

  def run(): Boolean
}
