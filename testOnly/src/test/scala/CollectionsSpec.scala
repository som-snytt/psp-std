package psp
package tests

import scala.collection.immutable.StringOps
import psp.std._, api._
import Prop.forAll
import StdEq._, StdShow._

class StringExtensions extends ScalacheckBundle {
  def bundle = "String Extensions"
  def s = "123 monkey dog ^^.* hello mother 456"

  implicit def arbWord: Arb[String] = Arb(gen.word)
  implicit def throwableEq: Eq[Throwable] = eqBy[Throwable](_.getClass)

  def scalaOps(s: String)  = new StringOps(s)
  def policyOps(s: String) = new PspStringOps(s)
  def sameBehavior[A: Eq](expr1: => A, expr2: => A): Prop = Try(expr1) === Try(expr2)

  def newProp[A: Eq](f: StringOps => A, g: String => A): Prop = forAll((s: String) => sameBehavior(f(scalaOps(s)), g(s)))

  def newProp2[B] = new {
    def apply[R](f: (StringOps, B) => R)(g: (String, B) => R)(implicit z1: Arb[B], z2: Eq[R]): Prop =
      forAll((s: String, x: B) => sameBehavior(f(scalaOps(s), x), g(s, x)))
  }

  // dropRight and takeRight have the domain limited because of a scala bug with
  // take/dropRight with values around MinInt.
  def mostInts = arb[Int] filter (_ > MinInt + 5000)

  def props: sciList[NamedProp] = sciList(
    "stripSuffix" -> newProp2[String](_ stripSuffix _)(_ stripSuffix _),
    "stripPrefix" -> newProp2[String](_ stripPrefix _)(_ stripPrefix _),
    "take"        -> newProp2[Int](_ take _)(_ take _ build),
    "drop"        -> newProp2[Int](_ drop _)(_ drop _ build),
    "takeRight"   -> newProp2[Int](_ takeRight _)(_ takeRight _ build)(mostInts, ?),
    "dropRight"   -> newProp2[Int](_ dropRight _)(_ dropRight _ build)(mostInts, ?),
    "toInt"       -> newProp[Int](_.toInt, _.toInt),
    "tail"        -> newProp[String](_.tail, _.tail.force),
    "head"        -> newProp(_.head, _.head),
    "drop"        -> newProp[Char](_.head, _.head),
    "reverse"     -> newProp[String](_.reverse, _.reverse.force)
  )
}

class GridSpec extends ScalacheckBundle {
  def bundle = "Policy, Grid Operations"

  def primePartition = Each from 2 mpartition (xs => _ % xs.head == 0)
  def primePartitionGrid(n: Int): View2D[Int]   = primePartition take n map (_ take n)
  def primePartitionGrid_t(n: Int): View2D[Int] = primePartition.transpose take n map (_ take n)
  def showGrid(xss: View2D[Int]): String = {
    val yss = xss mmap (_.to_s)
    val width = yss.flatten.map(_.length).max.size
    (yss mmap (x => width.leftFormatString format x) map (_ mk_s " ") mk_s "\n").trim.trimLines
  }
  def primePartition6 = sm"""
    |2   4   6   8   10  12
    |3   9   15  21  27  33
    |5   25  35  55  65  85
    |7   49  77  91  119 133
    |11  121 143 187 209 253
    |13  169 221 247 299 377
  """
  def primePartition6_t = sm"""
    |2   3   5   7   11  13
    |4   9   25  49  121 169
    |6   15  35  77  143 221
    |8   21  55  91  187 247
    |10  27  65  119 209 299
    |12  33  85  133 253 377
  """

  def props = sciList(
    seqShows("[ 2, 4, 6, ... ], [ 3, 9, 15, ... ], [ 5, 25, 35, ... ]", Each from 2 mpartition (xs => _ % xs.head == 0) take 3),
    showsAs(primePartition6, showGrid(primePartitionGrid(6))),
    showsAs(primePartition6_t, showGrid(primePartitionGrid_t(6)))
  )
}

class PolicyBasic extends ScalacheckBundle {
  def bundle = "Policy, Basic Collections Operations"

  def plist   = PolicyList(1, 2, 3)
  def pvector = Direct(1, 2, 3)
  def parray  = Array(1, 2, 3)
  def pseq    = Each[Int](parray foreach _)
  def punfold = Each from 1

  def closure   = parray transitiveClosure (x => exView(x.init.force, x.tail.force)) mk_s ", "
  def xxNumbers = (Each from 0).m grep """^(.*)\1""".r

  def props: sciList[NamedProp] = sciList(
    showsAs("[ 1, 2, 3 ]", plist),
    showsAs("[ 1, 2, 3 ]", pvector),
    showsAs("[ 1, 2, 3 ]", parray),
    showsAs("[ 1, 2, 3, 1, 2, 3 ]", plist ++ plist force),
    showsAs("[ 1, 2, 3, 1, 2, 3 ]", pvector ++ pvector force),
    showsAs("[ 1, 2, 3, 1, 2, 3 ]", parray ++ parray force),
    showsAs("[ 1, 2, 3, ... ]", punfold),
    showsAs("[ 1, 2, 3 ], [ 1, 2 ], [ 1 ], [  ], [ 2 ], [ 2, 3 ], [ 3 ]", closure),
    seqShows("1 -> 0, 2 -> 1, 3 -> 2", pvector.m.mapWithIndex(_ -> _)),
    seqShows("11, 22, 33, 44", indexRange(1, 50).pvec.m grep """(.)\1""".r),
    seqShows("99, 1010, 1111", xxNumbers drop 8 take 3)
  )
}

class CollectionsSpec extends ScalacheckBundle {
  def bundle = "Type Inference, General"

  val bits = sciBitSet(1, 2, 3)
  val arr  = Array[Int](1, 2, 3)
  val smap = sciMap("a" -> 1, "b" -> 2, "c" -> 3)
  val sseq = sciSeq("a" -> 1, "b" -> 2, "c" -> 3)
  val svec = sciVector("a" -> 1, "b" -> 2, "c" -> 3)
  val sset = sciSet("a" -> 1, "b" -> 2, "c" -> 3)

  def paired[A](x: A): (A, Int) = x -> ("" + x).length

  def props: sciList[NamedProp] = policyProps ++ sciList(
    expectTypes[sciBitSet](
      bits.m map identity build,
      bits.m map (_.toString.length) build,
      bits.m map (_.toString) map (_.length) build,
      bits.m map (x => sciList(x)) map (_.size) build,
      bits.m map (x => sciList(x).size) build
    ),
    expectTypes[String](
      "abc" map identity build,
      "abc" map (_.toInt.toChar) build,
      "abc".m flatMap (_.toString * 3 m) build,
      "abc" flatMap (_.toString * 3) build
    ),
    expectTypes[Array[Int]](
      arr mapInPlace identity,
      arr.m.build,
      arr.m flatMap (x => Direct(x)) build,
      arr.flatMap(x => Direct(x)).force[Array[Int]]
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

  def policyProps: sciList[NamedProp] = {
    import StdEq._
    val pset = exSet("a" -> 1, "b" -> 2, "c" -> 3)
    val pseq = exSeq("a" -> 1, "b" -> 2, "c" -> 3)

    sciList(
      expectTypes[ExSet[_]](
        pset.m map identity build,
        pset.m.build,
        pset.m map identity build,
        pset.m.map(_._1).map(paired).force[ExSet[_]]
      ),
      expectTypes[Each[_]](
        pseq map identity,
        pseq.m.build,
        pseq.m map identity build,
        pseq.m.map(_._1).map(paired).force[Each[_]]
      )
    )
  }
}

