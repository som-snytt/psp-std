package psp
package tests

import utest._

object NatSpec extends TestSuite {
  val list = core.NatList(1, 2, 3, 4)
  val tests = TestSuite {
    "zip2sum" - {
      val result = list zip list map (_ + _)
      assert(result.sum == (list.sum * 2))
    }
    "zip3sum" - {
      val result = list zip list zip list map (_ + _ + _)
      assert(result.sum == (list.sum * 3))
    }
  }
}
