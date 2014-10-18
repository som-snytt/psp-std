package psp
package tests

import psp.std._
import org.scalacheck.Test

// TODO - leverage now-available it-doesn't-typecheck test machinery.
class Nats extends Bundle {
  def ints = NatList((1, 2, 3, 4))
  def strs = NatList(("a", "ab", "abc", "abcd"))

  def run(): Boolean = {
    assert((ints zip ints map (_ + _)).sum == (ints.sum * 2))
    assert((ints zip ints zip ints map (_ + _ + _) sum) == ints.sum * 3)
    assert((ints zip strs zip ints map (_ + _.length + _) sum) == ints.sum * 3)
    finish()
    true
  }
}
