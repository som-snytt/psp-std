package psp
package tests

import scala.collection.immutable
import utest._
import utest.ExecutionContext.RunNow
import utest.framework.TestSuite
import psp.core.{ Foreach }

abstract class PspSpec extends org.specs2.mutable.Specification with org.specs2.matcher.ThrownExpectations {
  sequential
}

object ViewSpec extends TestSuite {
  // to keep utest happy since the macros are apparently relying on certain implicits
  val raisePspStringOps = null

  def checkResult[T: ClassTag](result: Object) = assert(classTag[T].runtimeClass isAssignableFrom result.getClass)
  implicit def selectiveStringConversion(s: String): Seq[Char] = s.toCharArray.toSeq

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
    "seq" - {
      checkResult[Seq[_]](Seq("a" -> 1, "b" -> 2, "c" -> 3).m.native)
      checkResult[Seq[_]](Seq("a" -> 1, "b" -> 2, "c" -> 3).m map (_._1) map (x => (x, x)) force)
    }
    "vector" - {
      checkResult[Vector[_]](Vector("a" -> 1, "b" -> 2, "c" -> 3).m.native)
      checkResult[Vector[_]](Vector("a" -> 1, "b" -> 2, "c" -> 3).m map (_._1) map (x => (x, x)) force)
    }
  }
}
