package psp
package tests

import psp.std._, api._

class Collections extends ScalacheckBundle {
  def bundle = "Collections"
  val bits = sciBitSet(1, 2, 3)
  val arr  = Array[Int](1, 2, 3)
  val smap = sciMap("a" -> 1, "b" -> 2, "c" -> 3)
  val sseq = scSeq("a" -> 1, "b" -> 2, "c" -> 3)
  val svec = sciVector("a" -> 1, "b" -> 2, "c" -> 3)
  val sset = sciSet("a" -> 1, "b" -> 2, "c" -> 3)

  def paired[A](x: A): (A, Int) = x -> ("" + x).length

  def props: Seq[NamedProp] = Seq(
    checkResults[Object, sciBitSet](
      bits map identity,
      bits.m map (_.toString.length) build,
      bits.m map (_.toString) map (_.length) build,
      bits.m map (x => Seq(x)) map (_.size) build,
      bits.m map (x => Seq(x).size) build
    ),
    checkResults[String, String](
      "abc" map identity,
      "abc" map (_.toInt.toChar),
      "abc".m flatMap (_.toString * 3 m) build,
      "abc" flatMap (_.toString * 3)
    ),
    checkResults[Object, Array[Int]](
      arr mapInPlace identity,
      arr.m.build,
      arr.m flatMap (x => Foreach elems x) build,
      arr.flatMap(x => Foreach elems x).force[Array[Int]]
    ),
    checkResults[Object, sciSet[_]](
      sset map identity,
      sset.m build,
      sset.m map identity build,
      sset.m.map(_._1) map paired build
    ),
    checkResults[Object, sciMap[_, _]](
      (smap map identity).force[sciMap[_, _]],
      smap.m build,
      smap.m map identity build,
      smap.m map (_._1) map identity map paired build
    ),
    checkResults[Object, scSeq[_]](
      sseq map identity,
      sseq.m build,
      sseq.m map identity build,
      sseq.m.map(_._1).map(paired).force[scSeq[_]]
    ),
    checkResults[Object, sciVector[_]](
      svec map identity,
      svec.m.build,
      svec.m map identity build,
      svec.m.map(_._1).map(paired).force[sciVector[_]]
    )
  )
}
