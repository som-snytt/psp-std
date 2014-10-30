package psp
package tests

import psp.std._, api._, StdEq._, StdShow._

class SliceSpec extends ScalacheckBundle {
  def bundle = "Slice Operations"
  def checkSlice[A : Eq : Show](xs: Direct[A], start: Int, end: Int, expect: Direct[A]): sciList[NamedProp] = sciList(
    show"$xs.slice($start, $end) === $expect"              -> Prop((xs slice indexRange(start, end) force) === expect),
    show"$xs drop $start take ($end - $start) === $expect" -> Prop((xs drop newSize(start) take newSize(end - start) force) === expect)
  )

  def props = sciList(
    checkSlice('a' to 'g', 2, 5, 'c' to 'e')
  ).flatten
}

class InferenceSpec extends ScalacheckBundle {
  def bundle = "Type Inference, Views"

  val as: Array[Int]     = Array(1, 2, 3)
  val ds: Direct[Int]   = Direct(1, 2, 3)
  val fs: Each[Int]      = Each(ds foreach _)
  val ls: sciList[Int]   = sciList(1, 2, 3)
  val ss: String         = "123"
  val vs: sciVector[Int] = sciVector(1, 2, 3)
  val xs: exSet[Int]     = exSet(1, 2, 3)

  val b1 = as map identity build
  val b2 = ds map identity build
  val b3 = fs map identity build
  val b4 = ls.m map identity build
  val b5 = ss map identity build
  val b6 = vs.m map identity build
  // val b7 = xs map identity //build

  def ptBuild = sciList[NamedProp](
    expectType[Array[Int]](b1),
    expectType[Direct[Int]](b2),
    expectType[Each[Int]](b3),
    expectType[sciList[Int]](b4),
    expectType[String](b5),
    expectType[sciVector[Int]](b6)
    // expectType[exSet[Int]](b7)
  )
  def ptArray = expectTypes[Array[Int]](
    as.m map identity build,
    as.m map identity force,
    ds.m map identity force,
    fs.m map identity force,
    ls.m map identity force,
    vs.m map identity force,
    xs.m map identity force,
    as map identity build,
    as map identity force
  )
  def ptView = expectTypes[View[Int]](
    as.m map identity,
    ds.m map identity,
    fs.m map identity,
    ls.m map identity,
    ss.m map identity map (_.toInt),
    vs.m map identity,
    xs.m map identity,
    as map identity,
    ds map identity,
    fs map identity,
    ls map identity,
    ss map identity map (_.toInt),
    vs map identity
    // xs map identity
  )
  def ptVector = expectTypes[sciVector[Int]](
    as.m map identity force,
    ds.m map identity force,
    fs.m map identity force,
    ls.m map identity force,
    vs.m map identity build,
    vs.m map identity force,
    xs.m map identity force
  )

  def props: sciList[NamedProp] = sciList(ptArray, ptView, ptVector) ++ ptBuild ++ sciList(
    expectType[Array[Char]]   (ss.m map identity force),
    // expectType[String]        (ss map identity),
    expectType[String]        (ss map identity build),
    // expectType[String]        (ss.m map identity),
    expectType[String]        (ss.m map identity force),
    expectType[View[Char]]    (ss.m map identity),
    expectType[exSet[Int]]    (xs.m map identity force),
    expectType[Each[Int]]     (fs map identity),
    expectType[Each[Int]]     (fs.m map identity force),
    expectType[Direct[Int]]  (ds map identity),
    expectType[Direct[Int]]  (ds.m map identity force),
    expectType[Direct[Int]]  (vs.m map identity force),
    expectType[sciList[Int]]  (ls map identity),
    expectType[sciList[Int]]  (ls.m map identity force),
    expectType[sciVector[Int]](vs map identity)
  )
}
