package psp
package core
package tests

import org.specs2.{ mutable => mu }
import scala.util.Random.nextInt
import org.specs2.matcher.ThrownExpectations
import scala.collection.{ mutable, immutable, generic }
import immutable.BitSet

class ViewSpec extends PspSpec {
  private def check(what: String, cond: Boolean): Boolean = try cond finally what in cond

  def bitsetTest(): Boolean = {
    def f1 = BitSet(1, 2, 3).m map (_.toString.length) force
    def f2 = BitSet(1, 2, 3).m map (_.toString) map (_.length) force
    def f3 = BitSet(1, 2, 3).m map (x => Seq(x)) map (_.size) force
    def f4 = BitSet(1, 2, 3).m map (x => Seq(x).size) force

    List(f1, f2, f3, f4).zipWithIndex forall { case (res, i) =>
      check(s"Bitset expression ${i + 1} results in BitSet", res.getClass.getName contains "BitSet")
    }
  }

  def stringTest(): Boolean = {
    def f1 = "abc".m map (_.toInt.toChar) force
    def f2 = "abc".m flatMap (_.toString * 3) force

    List(f1, f2).zipWithIndex forall { case (res, i) =>
      check(s"String expression ${i + 1} results in String", res.getClass == classOf[String])
    }
  }

  def arrayTest(): Boolean = {
    def f1 = Array[Int](1, 2, 3).m flatMap (x => Vector(x)) force

    List(f1).zipWithIndex forall { case (res, i) =>
      check(s"Array[Int] expression ${i + 1} results in Array", res.getClass == classOf[Array[Int]])
    }
  }

  def mapTest(): Boolean = {
    def f1 = Map("a" -> 1, "b" -> 2, "c" -> 3).m map (_._1) map (x => (x, x)) force

    List(f1).zipWithIndex forall { case (res, i) =>
      check(s"Map expression ${i + 1} results in Map", res.getClass.getName contains "scala.collection.immutable.Map")
    }
  }

  def seqTest(): Boolean = {
    def f1 = Seq("a" -> 1, "b" -> 2, "c" -> 3).m map (_._1) map (x => (x, x)) force

    List(f1).zipWithIndex forall { case (res, i) =>
      check(s"Seq expression ${i + 1} results in Seq", classOf[Seq[_]] isAssignableFrom res.getClass)
    }
  }

  bitsetTest()
  stringTest()
  arrayTest()
  mapTest()
  seqTest()
}
