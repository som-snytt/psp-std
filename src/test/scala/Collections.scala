package psp
package tests

import scala.collection.immutable
import psp.core._
import psp.std._

class Collections extends Bundle {
  def checkResult[T: ClassTag](result: Object) = assert(classTag[T].runtimeClass isAssignableFrom result.getClass)

  val xs = immutable.BitSet(1, 2, 3)

  def run(): Boolean = {
    checkResult[immutable.BitSet](xs.m map (_.toString.length) native)
    checkResult[immutable.BitSet](xs.m map (_.toString) map (_.length) native)
    checkResult[immutable.BitSet](xs.m map (x => Seq(x)) map (_.size) native)
    checkResult[immutable.BitSet](xs.m map (x => Seq(x).size) native)
    checkResult[String]("abc".m map (_.toInt.toChar) native)
    checkResult[String]("abc".m flatMap (_.toString * 3 m) native)
    checkResult[Array[Int]](Array[Int](1, 2, 3).m.native)
    checkResult[Array[Int]](Array[Int](1, 2, 3).m flatMap (x => Foreach elems x) native)
    checkResult[immutable.Map[_, _]](immutable.Map("a" -> 1, "b" -> 2, "c" -> 3).m.native)
    checkResult[immutable.Map[_, _]](immutable.Map("a" -> 1, "b" -> 2, "c" -> 3).m map (x => x) native)
    checkResult[Seq[_]](Seq("a" -> 1, "b" -> 2, "c" -> 3).m.native)
    checkResult[Seq[_]]((Seq("a" -> 1, "b" -> 2, "c" -> 3).m map (_._1) map (x => (x, x))).force[Seq[_]])
    checkResult[Vector[_]](Vector("a" -> 1, "b" -> 2, "c" -> 3).m.native)
    checkResult[Vector[_]]((Vector("a" -> 1, "b" -> 2, "c" -> 3).m map (_._1) map (x => (x, x))).force[Vector[_]])
    finish()
  }
}
