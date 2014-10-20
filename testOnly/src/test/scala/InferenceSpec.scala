package psp
package tests

import psp.std._, api._, StdEq._, StdShow._

class SliceSpec extends ScalacheckBundle {
  def bundle = "Slice Operations"
  def checkSlice[A : Eq : Show](xs: pVector[A], start: Int, end: Int, expect: pVector[A]): Seq[NamedProp] = Seq(
    show"$xs.slice($start, $end) === $expect"              -> Prop((xs slice indexRange(start, end) force) === expect),
    show"$xs drop $start take ($end - $start) === $expect" -> Prop((xs drop newSize(start) take newSize(end - start) force) === expect)
  )

  def props = Seq(
    checkSlice('a' to 'g', 2, 5, 'c' to 'e')
  ).flatten
}

class InferenceSpec extends ScalacheckBundle {
  def bundle = "Type Inference, Views"

  val as: Array[Int]     = Array(1, 2, 3)
  val ds: pVector[Int]   = Direct(1, 2, 3)
  val fs: pSeq[Int]      = Foreach(ds foreach _)
  val ls: sciList[Int]   = sciList(1, 2, 3)
  val ss: String         = "123"
  val vs: sciVector[Int] = sciVector(1, 2, 3)
  val xs: exSet[Int]     = newSet(1, 2, 3)

  def ptArray = expectTypes[Array[Int]](
    as.m map identity build,
    ds.m map identity force,
    fs.m map identity force,
    ls.m map identity force,
    vs.m map identity force,
    xs.m map identity force,
    as map identity
  )
  def ptView = expectTypes[View[Int]](
    as.m map identity,
    ds.m map identity,
    fs.m map identity,
    ls.m map identity,
    vs.m map identity,
    xs.m map identity
  )
  def ptVector = expectTypes[sciVector[Int]](
    as.m map identity force,
    ds.m map identity force,
    fs.m map identity force,
    ls.m map identity force,
    vs.m map identity build,
    xs.m map identity force
  )

  def props: Seq[NamedProp] = Seq(ptArray, ptView, ptVector) ++ Seq(
    expectType[Array[Char]]   (ss.m map identity force),
    expectType[String]        (ss map identity),
    expectType[String]        (ss.m map identity force),
    expectType[View[Char]]    (ss.m map identity),
    expectType[exSet[Int]]    (xs.m map identity force),
    expectType[pSeq[Int]]     (fs map identity),
    expectType[pSeq[Int]]     (fs.m map identity force),
    expectType[pVector[Int]]  (ds map identity),
    expectType[pVector[Int]]  (ds.m map identity force),
    expectType[pVector[Int]]  (vs.m map identity force),
    expectType[sciList[Int]]  (ls map identity),
    expectType[sciList[Int]]  (ls.m map identity force),
    expectType[sciVector[Int]](vs map identity)
  )
}
