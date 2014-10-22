package psp
package tests

import psp.std._, api._
import org.scalacheck._, Prop._
import StdEq._

class StringExtensions extends ScalacheckBundle {
  import scala.collection.immutable.StringOps
  import StdEq._

  def bundle = "String Extensions"
  val s = "123 monkey dog ^^.* hello mother 456"
  implicit def arbWord: Arbitrary[String] = Arbitrary(genWord)
  implicit def throwableEq: Eq[Throwable] = eqBy[Throwable](_.getClass)

  def scalaOps(s: String)  = new StringOps(s)
  def policyOps(s: String) = new PspStringOps(s)
  def sameBehavior[A: Eq](expr1: => A, expr2: => A): Prop = Try(expr1) === Try(expr2)

  def newProp[A: Eq](f: StringOps => A, g: String => A): Prop = forAll((s: String) => sameBehavior(f(scalaOps(s)), g(s)))

  def newProp2[B] = new {
    def apply[R](f: (StringOps, B) => R)(g: (String, B) => R)(implicit z1: Arbitrary[B], z2: Eq[R]): Prop =
      forAll((s: String, x: B) => sameBehavior(f(scalaOps(s), x), g(s, x)))
  }

  // dropRight and takeRight have the domain limited because of a scala bug with
  // take/dropRight with values around MinInt.
  def mostInts = implicitly[Arbitrary[Int]] filter (_ > MinInt + 5000)

  def props: Seq[NamedProp] = Seq(
    "stripSuffix" -> newProp2[String](_ stripSuffix _)(_ stripSuffix _),
    "stripPrefix" -> newProp2[String](_ stripPrefix _)(_ stripPrefix _),
    "take"        -> newProp2[Int](_ take _)(_ take _),
    "drop"        -> newProp2[Int](_ drop _)(_ drop _),
    "takeRight"   -> newProp2[Int](_ takeRight _)(_ takeRight _)(mostInts, ?),
    "dropRight"   -> newProp2[Int](_ dropRight _)(_ dropRight _)(mostInts, ?),
    "toInt"       -> newProp[Int](_.toInt, _.toInt),
    "tail"        -> newProp[String](_.tail, _.m.tail.force),
    "head"        -> newProp(_.head, _.head),
    "drop"        -> newProp[Char](_.head, _.head),
    "reverse"     -> newProp[String](_.reverse, _.reverse.force)
  )
}

class PolicyBasic extends ScalacheckBundle {
  def bundle = "Policy, Basic Collections Operations"
  import StdShow._

  def plist   = PolicyList(1, 2, 3)
  def pvector = Direct(1, 2, 3)
  def parray  = Array(1, 2, 3)
  def pseq    = Foreach[Int](parray foreach _)
  def punfold = Foreach from 1

  def showsAs[A: Show](expected: String, x: A): NamedProp = expected -> (expected =? show"$x")

  def closure = parray transitiveClosure (x => Direct(x.m.init, x.m.tail)) mk_s ", "

  def props: Seq[NamedProp] = Seq(
    showsAs("[ 1, 2, 3 ]", plist),
    showsAs("[ 1, 2, 3 ]", pvector),
    showsAs("[ 1, 2, 3 ]", parray),
    showsAs("[ 1, 2, 3 ] ++ [ 1, 2, 3 ]", plist ++ plist),
    showsAs("[ 1, 2, 3, 1, 2, 3 ]", pvector ++ pvector),
    showsAs("[ 1, 2, 3, 1, 2, 3 ]", parray ++ parray),
    showsAs("[ 1, 2, 3, ... ]", punfold),
    showsAs("[ 1, 2, 3 ], [ 1, 2 ], [ 1 ], [  ], [ 2 ], [ 2, 3 ], [ 3 ]", closure)
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
    val pset = newSet("a" -> 1, "b" -> 2, "c" -> 3)
    val pseq = newSeq("a" -> 1, "b" -> 2, "c" -> 3)

    Seq(
      expectTypes[exSet[_]](
        pset.m map identity build,
        pset.m.build,
        pset.m map identity build,
        pset.m.map(_._1).map(paired).force[exSet[_]]
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

