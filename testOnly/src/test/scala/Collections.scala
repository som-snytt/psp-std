package psp
package tests

import psp.std._, api._

class Collections extends Bundle {
  val xs = scala.collection.immutable.BitSet(1, 2, 3)
  def checkResult[T: ClassTag](result: Object) = assert(classTag[T].runtimeClass isAssignableFrom result.getClass)

  // implicit def convertCanBuild[Elem, To](implicit z: CanBuild[Elem, To]): Builds[Elem, To] = Builds wrap z

  def run(): Boolean = {
    checkResult[sciBitSet](xs.m map (_.toString.length) native)
    checkResult[sciBitSet](xs.m map (_.toString) map (_.length) native)
    checkResult[sciBitSet](xs.m map (x => Seq(x)) map (_.size) native)
    checkResult[sciBitSet](xs.m map (x => Seq(x).size) native)
    checkResult[String]("abc".m map (_.toInt.toChar) native)
    checkResult[String]("abc".m flatMap (_.toString * 3 m) native)
    checkResult[Array[Int]](Array[Int](1, 2, 3).m.native)
    checkResult[Array[Int]](Array[Int](1, 2, 3).m flatMap (x => Foreach elems x) native)
    checkResult[sciMap[_, _]](sciMap("a" -> 1, "b" -> 2, "c" -> 3).m.native)
    checkResult[sciMap[_, _]](sciMap("a" -> 1, "b" -> 2, "c" -> 3).m map (x => x) native)
    checkResult[Seq[_]](Seq("a" -> 1, "b" -> 2, "c" -> 3).m.native)
    checkResult[Seq[_]]((Seq("a" -> 1, "b" -> 2, "c" -> 3).m map (_._1) map (x => (x, x))).force[Seq[_]])
    checkResult[Vector[_]](Vector("a" -> 1, "b" -> 2, "c" -> 3).m.native)
    checkResult[Vector[_]]((Vector("a" -> 1, "b" -> 2, "c" -> 3).m map (_._1) map (x => (x, x))).force[Vector[_]])
    finish()
  }
}
