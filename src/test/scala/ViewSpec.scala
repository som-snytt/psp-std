package psp
package core
package tests

import org.specs2.{ mutable => mu }
import scala.util.Random.nextInt
import org.specs2.matcher.ThrownExpectations
import scala.collection.{ mutable, immutable, generic }
import immutable.BitSet
import scala.reflect.{ ClassTag, classTag }

class ViewSpec extends PspSpec {
  private def check(what: String, cond: Boolean): Boolean = try cond finally what in cond
  private def checkResult[T: ClassTag](pair: (Object, Int)): Boolean = {
    val (result, index) = pair
    val expect = classTag[T].runtimeClass
    val actual = result.getClass
    val cname = expect.getName split "[.]" last
    val rname = actual.getName split "[.]" last
    val ok = expect isAssignableFrom actual
    val word = if (ok) "[pass]" else "[fail]"
    val str = if (ok) s"$word $cname expression ${index + 1} results in $rname" else s"$word ${expect.getName} expression ${index + 1} results in ${actual.getName}"

    check(str, ok)
  }

  private def allTrue(tests: List[Boolean]) = tests forall (x => x)

  def bitsetTest(): Boolean = {
    def f1 = BitSet(1, 2, 3).m map (_.toString.length) native
    def f2 = BitSet(1, 2, 3).m map (_.toString) map (_.length) native
    def f3 = BitSet(1, 2, 3).m map (x => Seq(x)) map (_.size) native
    def f4 = BitSet(1, 2, 3).m map (x => Seq(x).size) native

    allTrue(List(f1, f2, f3, f4).zipWithIndex map (x => checkResult[BitSet](x)))
  }

  def stringTest(): Boolean = {
    def f1 = "abc".m map (_.toInt.toChar) native
    def f2 = "abc".m flatMap (_.toString * 3) native

    allTrue(List(f1, f2).zipWithIndex map (x => checkResult[String](x)))
  }

  def arrayTest(): Boolean = {
    def f1 = Array[Int](1, 2, 3).m.native
    def f2 = Array[Int](1, 2, 3).m flatMap (x => Foreach.elems(x)) native

    allTrue(List(f1, f2).zipWithIndex map (x => checkResult[Array[Int]](x)))
  }

  def mapTest(): Boolean = {
    def f1 = immutable.Map("a" -> 1, "b" -> 2, "c" -> 3).m map (x => x) native
    def f2 = immutable.Map("a" -> 1, "b" -> 2, "c" -> 3).m.native
    def f3 = immutable.Map("a" -> 1, "b" -> 2, "c" -> 3).m map (_._1) map (x => (x, x)) force

    allTrue(List(f1, f2, f3).zipWithIndex map (x => checkResult[immutable.Map[_,_]](x)))
  }

  def seqTest(): Boolean = {
    def f1 = Seq("a" -> 1, "b" -> 2, "c" -> 3).m.force
    def f2 = Seq("a" -> 1, "b" -> 2, "c" -> 3).m map (_._1) map (x => (x, x)) force

    allTrue(List(f1, f2).zipWithIndex map (x => checkResult[Seq[_]](x)))
  }

  def vectorTest(): Boolean = {
    def f1 = Vector("a" -> 1, "b" -> 2, "c" -> 3).m.force
    def f2 = Vector("a" -> 1, "b" -> 2, "c" -> 3).m.force// map (_._1) map (x => (x, x)) force

    allTrue(List(f1, f2).zipWithIndex map (x => checkResult[Vector[_]](x)))
  }

  bitsetTest()
  stringTest()
  arrayTest()
  mapTest()
  seqTest()
  vectorTest()
}
