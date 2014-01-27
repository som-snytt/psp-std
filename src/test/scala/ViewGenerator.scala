package psp
package core
package tests

import org.specs2.{ mutable => mu }
import compat.ScalaNative

class CollectionAndCompositeOp[A](xs: ElementalView[A], f: ElementalView[A] => ElementalView[A]) {
  val view   = f(xs)
  val result = view take 3 mk_s ", "
  val count  = xs.calls
  override def toString = pp"$view"
}

class PreciseSpec extends PspSpec {
  runCollectionsTests()

  lazy val tupleFlatMap: Int => Foreach[Int] = ((x: Int) => Foreach.elems(x, x)) labeled "(x, x)"
  lazy val isEven: Int => Boolean            = ((x: Int) => x % 2 == 0) labeled "isEven"
  lazy val timesThree: Int => Int            = ((x: Int) => x * 3) labeled "*3"

  def max    = 1000
  def numOps = 3
  def basicOps = List[ElementalView[Int] => ElementalView[Int]](
    _ drop 5,
    _ dropRight 11,
    _.slice(7, 41),
    _ take 13,
    _ takeRight 17,
    _ flatMap tupleFlatMap,
    _ filter isEven,
    _ map timesThree
  )

  def scalaIntRange: scala.collection.immutable.Range = Range.inclusive(1, max, 1)

  def rootCollections  = Vector[ElementalView[Int]](
    PspList.to(1, max).m,
    PspList.to(1, max).m sized Size(max),
    IntRange.to(1, max).m,
    ScalaNative(scalaIntRange.toList),
    ScalaNative(scalaIntRange.toList.view),
    ScalaNative(scalaIntRange.toStream),
    ScalaNative(scalaIntRange.toStream.view),
    ScalaNative(scalaIntRange.view),
    ScalaNative(scalaIntRange.toVector.view)
  )

  def compositesOfN(n: Int): List[ElementalView[Int] => ElementalView[Int]] = (
    (basicOps combinations n flatMap (_.permutations.toList)).toList.distinct
      map (xss => xss reduceLeft (_ andThen _))
  )

  def runCollectionsTests() {
    val banner: String = List("Psp/L", "Psp/LS", "Psp/I", "List", "List/V", "Stream", "Strm/V", "Rng/V", "Vctr/V") map ("%6s" format _) mkString " "
    val underline: String = banner.toCharArray map (ch => if (ch == ' ') ' ' else '-') mkString ""

    val composites                                          = compositesOfN(numOps)
    val testss: List[Vector[CollectionAndCompositeOp[Int]]] = composites map (op => rootCollections map (xs => new CollectionAndCompositeOp(xs, op)))
    val lines: List[String] = testss sortBy (xs => ((xs(1).count, xs(0).count))) map { tests =>
      val counts = tests map ("%6s" format _.count) mkString " "
      val res = tests.map(_.result).distinct.m.toPspList match {
        case s :: nil() => s
        case ss         => "!!! " + tests.take(3).zipWithIndex.map({ case (x, i) => s"xs($i) = ${x.result}" }).mk_s("  /  ") //ss.mk_s(" ... ")
      }
      "%s     %s  //  %s".format(tests.head, counts, res)
    }
    val padding = " " * (testss.head.head.toString.length + 5)
    println(padding + banner)
    println(padding + underline)
    lines foreach println
  }
}
