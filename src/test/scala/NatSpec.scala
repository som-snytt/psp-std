package psp
package core
package tests

class NatSpec extends PspSpec {
  "natlist" should {
    val list = NatList(1, 2, 3, 4)

    "(xs zip xs).sum == xs.sum * 2" in {
      val result = list zip list map (_ + _) sum;
      result must equalTo(list.sum * 2)
    }

    "(xs zip xs zip xs).sum == xs.sum * 3" in {
      val result = list zip list zip list map (_ + _ + _) sum;
      result must equalTo(list.sum * 3)
    }
  }
}
