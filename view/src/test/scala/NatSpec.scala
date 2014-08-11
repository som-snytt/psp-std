package psp
package tests

import utest._
import psp.core._

// TODO - leverage now-available it-doesn't-typecheck test machinery.
object NatSpec extends TestSuite {
  def ints = NatList(1, 2, 3, 4)
  def strs = NatList("a", "ab", "abc", "abcd")

  val tests = TestSuite {
    "zip2sum" - {
      val result = ints zip ints map (_ + _)
      assert(result.sum == (ints.sum * 2))
    }
    "zip3sum" - {
      val result = ints zip ints zip ints map (_ + _ + _)
      assert(result.sum == (ints.sum * 3))
    }
    "zip3sum2" - {
      val result = ints zip strs zip ints map (_ + _.length + _)
      assert(result.sum == (ints.sum * 3))
    }
  }
}
