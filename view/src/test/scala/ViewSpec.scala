package psp
package tests

import scala.collection.immutable
import utest._
import psp.core._
import psp.std._

object ViewSpec extends TestSuite {
  def checkResult[T: ClassTag](result: Object) = assert(classTag[T].runtimeClass isAssignableFrom result.getClass)

  val tests = TestSuite {
    "bitset" - {
      val xs = immutable.BitSet(1, 2, 3)
      checkResult[immutable.BitSet](xs.m map (_.toString.length) native)
      checkResult[immutable.BitSet](xs.m map (_.toString) map (_.length) native)
      checkResult[immutable.BitSet](xs.m map (x => Seq(x)) map (_.size) native)
      checkResult[immutable.BitSet](xs.m map (x => Seq(x).size) native)
    }
    "string" - {
      checkResult[String]("abc".m map (_.toInt.toChar) native)
      checkResult[String]("abc".m flatMap (_.toString * 3 m) native)
    }
    "array" - {
      checkResult[Array[Int]](Array[Int](1, 2, 3).m.native)
      checkResult[Array[Int]](Array[Int](1, 2, 3).m flatMap (x => Foreach elems x) native)
    }
    "map" - {
      checkResult[immutable.Map[_, _]](immutable.Map("a" -> 1, "b" -> 2, "c" -> 3).m.native)
      checkResult[immutable.Map[_, _]](immutable.Map("a" -> 1, "b" -> 2, "c" -> 3).m map (x => x) native)
    }
    // The balance of inference and implicits which allowed force to be called with no type argument
    // suddenly collapsed, don't know why.
    //
    // [error] /mirror/r/psp-view/src/test/scala/ViewSpec.scala:33: could not find implicit value for parameter pcb: psp.core.Builds[(String, String),That]
    // [error]       checkResult[Seq[_]](Seq("a" -> 1, "b" -> 2, "c" -> 3).m map (_._1) map (x => (x, x)) force)
    // [error]                                                                                            ^
    // [error] /mirror/r/psp-view/src/test/scala/ViewSpec.scala:37: could not find implicit value for parameter pcb: psp.core.Builds[(String, String),That]
    // [error]       checkResult[Vector[_]](Vector("a" -> 1, "b" -> 2, "c" -> 3).m map (_._1) map (x => (x, x)) force)
    // [error]                                                                                                  ^
    // [error] two errors found
    "seq" - {
      checkResult[Seq[_]](Seq("a" -> 1, "b" -> 2, "c" -> 3).m.native)
      checkResult[Seq[_]]((Seq("a" -> 1, "b" -> 2, "c" -> 3).m map (_._1) map (x => (x, x))).force[Seq[_]])
    }
    "vector" - {
      checkResult[Vector[_]](Vector("a" -> 1, "b" -> 2, "c" -> 3).m.native)
      checkResult[Vector[_]]((Vector("a" -> 1, "b" -> 2, "c" -> 3).m map (_._1) map (x => (x, x))).force[Vector[_]])
    }
  }
}
