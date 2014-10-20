package psp
package tests

import psp.std._, api._
import org.scalacheck.Prop._

class PolicyBasic extends ScalacheckBundle {
  def bundle = "Policy, Basic Collections Operations"

  import StdShow._

  def plist   = PolicyList(1, 2, 3)
  def pvector = Direct(1, 2, 3)
  def parray  = Array(1, 2, 3)
  def pseq    = Foreach[Int](parray foreach _)
  def punfold = Foreach from 1

  // def shown[A](xs: Foreach[A]): String = xs.to_s

  def showsAs[A: Show](expected: String, x: A): NamedProp = expected -> (expected =? show"$x")

  def props: Seq[NamedProp] = Seq(
    showsAs("[ 1, 2, 3 ]", plist),
    showsAs("[ 1, 2, 3 ]", pvector),
    showsAs("[ 1, 2, 3 ]", parray),
    showsAs("[ 1, 2, 3 ] ++ [ 1, 2, 3 ]", plist ++ plist),
    showsAs("[ 1, 2, 3, 1, 2, 3 ]", pvector ++ pvector),
    showsAs("[ 1, 2, 3, 1, 2, 3 ]", parray ++ parray),
    showsAs("[ 1, 2, 3, ... ]", punfold)
  )
}

class Collections extends ScalacheckBundle {
  def bundle = "Type Inference, General"

  val bits = sciBitSet(1, 2, 3)
  val arr  = Array[Int](1, 2, 3)
  val smap = sciMap("a" -> 1, "b" -> 2, "c" -> 3)
  val sseq = scSeq("a" -> 1, "b" -> 2, "c" -> 3)
  val svec = sciVector("a" -> 1, "b" -> 2, "c" -> 3)
  val sset = sciSet("a" -> 1, "b" -> 2, "c" -> 3)

  def paired[A](x: A): (A, Int) = x -> ("" + x).length

  def props: Seq[NamedProp] = policyProps ++ Seq(
    expectTypes[sciBitSet](
      bits map identity,
      bits.m map (_.toString.length) build,
      bits.m map (_.toString) map (_.length) build,
      bits.m map (x => Seq(x)) map (_.size) build,
      bits.m map (x => Seq(x).size) build
    ),
    expectTypes[String](
      "abc" map identity,
      "abc" map (_.toInt.toChar),
      "abc".m flatMap (_.toString * 3 m) build,
      "abc" flatMap (_.toString * 3)
    ),
    expectTypes[Array[Int]](
      arr mapInPlace identity,
      arr.m.build,
      arr.m flatMap (x => fromElems(x)) build,
      arr.flatMap(x => fromElems(x)).force[Array[Int]]
    ),
    expectTypes[sciSet[_]](
      sset map identity,
      sset.m build,
      sset.m map identity build,
      sset.m.map(_._1) map paired build
    ),
    expectTypes[sciMap[_, _]](
      (smap map identity).force[sciMap[_, _]],
      smap.m build,
      smap.m map identity build,
      smap.m map (_._1) map identity map paired build
    ),
    expectTypes[scSeq[_]](
      sseq map identity,
      sseq.m build,
      sseq.m map identity build,
      sseq.m.map(_._1).map(paired).force[scSeq[_]]
    ),
    expectTypes[sciVector[_]](
      svec map identity,
      svec.m.build,
      svec.m map identity build,
      svec.m.map(_._1).map(paired).force[sciVector[_]]
    )
  )

  def policyProps: Seq[NamedProp] = {
    import StdEq._
    // val pmap = newMap("a" -> 1, "b" -> 2, "c" -> 3)
    val pset = newSet("a" -> 1, "b" -> 2, "c" -> 3)
    val pseq = newSeq("a" -> 1, "b" -> 2, "c" -> 3)

    Seq(
      // expectTypes[Object, pMap[_, _]](
      //   pmap map identity,
      //   pmap.m.build,
      //   pmap.m map identity build,
      //   pmap.m.map(_._1).map(paired).force[pMap[_, _]]
      // ),
      expectTypes[pSet[_]](
        pset.m map identity build,
        pset.m.build,
        pset.m map identity build,
        pset.m.map(_._1).map(paired).force[pSet[_]]
      ),
      expectTypes[pSeq[_]](
        pseq map identity,
        pseq.m.build,
        pseq.m map identity build,
        pseq.m.map(_._1).map(paired).force[pSeq[_]]
      )
    )
  }
}
