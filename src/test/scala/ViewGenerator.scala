package psp
package core
package tests

import org.specs2.{ mutable => mu }
import compat.ScalaNative

class PreciseSpec extends PspSpec {
  runCollectionsTests()

  lazy val tupleFlatMap: Int => Foreach[Int] = ((x: Int) => Foreach.elems(x, x)) labeled "(x, x)"
  lazy val isEven: Int => Boolean            = ((x: Int) => x % 2 == 0) labeled "isEven"
  lazy val timesThree: Int => Int            = ((x: Int) => x * 3) labeled "*3"
  lazy val collectDivSix: Int =?> Int        = labelpf("%/6")({ case x: Int if x % 6 == 0 => x / 6 })

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
    _ map timesThree,
    _ collect collectDivSix
  )

  def scalaIntRange: scala.collection.immutable.Range = Range.inclusive(1, max, 1)

  def usCollections = List[ElementalView[Int]](
    PspList.to(1, max).m,
    PspList.to(1, max).m sized Size(max),
    IntRange.to(1, max).m
  )
  def themCollections = List[ElementalView[Int]](
    ScalaNative(scalaIntRange.toList.view),
    ScalaNative(scalaIntRange.toStream),
    ScalaNative(scalaIntRange.toStream.view),
    ScalaNative(scalaIntRange.view),
    ScalaNative(scalaIntRange.toVector.view)
  )
  def rootCollections = usCollections ++ themCollections

  def compositesOfN(n: Int): List[ElementalView[Int] => ElementalView[Int]] = (
    (basicOps combinations n flatMap (_.permutations.toList)).toList.distinct
      map (xss => xss reduceLeft (_ andThen _))
  )


  class CompositeOp(viewFn: ElementalView[Int] => ElementalView[Int]) {
    class CollectionResult(xs: ElementalView[Int]) {
      val view   = viewFn(xs)
      val result = view take 3 mk_s ", "
      val count  = xs.calls

      override def toString = pp"$view"
    }
    val us: List[CollectionResult]   = usCollections map (xs => new CollectionResult(xs))
    val control: CollectionResult    = new CollectionResult(ScalaNative(scalaIntRange.toList))
    val them: List[CollectionResult] = themCollections map (xs => new CollectionResult(xs))
    val all: List[CollectionResult]  = us ++ (control +: them)

    def usCounts   = us map (_.count)
    def themCounts = them map (_.count)
    def allResults = all map (_.result)
    def allCounts  = all map (_.count)

    def usAverage   = usCounts.sum / us.size.toDouble
    def themAverage = themCounts.sum / them.size.toDouble
    def ratioDouble = themAverage / usAverage
    def ratio       = "%.2f" format ratioDouble

    def headResult  = us.head
    def headView    = us.head.view
    def isAgreement = allResults.distinct.size == 1
    def display     = (
         !isAgreement
      || (usCounts.distinct.size == usCollections.size)
      || (allCounts.distinct.size > 4)
      || (allCounts.distinct.size > 2 && ratioDouble < 1.3d)
    )
    def countsString   = allCounts map ("%7s" format _) mkString " "
    def resultsString  = if (isAgreement) headResult.result else "!!! " + failedString
    def failedString   = {
      val grouped = all.zipWithIndex groupBy { case (x, i) => x.result }
      val lines = grouped.toList map { case (res, pairs) => "%20s:  %s".format(pairs.map(_._2).mkString(" "), res) }
      lines.mkString("\n  ", "\n  ", "\n")
    }
    def padding        = " " * (headView.toString.length + 2)
    def sortKey        = ((-ratio.toDouble, usCounts.min))

    override def toString = "%s  %6s %s  //  %s".format(headView, ratio, countsString, resultsString)
  }

  def runCollectionsTests() {
    val banner: String    = List("Improve", "Psp/L", "Psp/LS", "Psp/I", "<EAGER>", "ListV", "Stream", "StreamV", "RangeV", "VectorV") map ("%7s" format _) mkString " "
    val underline: String = banner.toCharArray map (ch => if (ch == ' ') ' ' else '-') mkString ""

    val composites     = compositesOfN(numOps)
    val results        = composites map (fn => new CompositeOp(fn))
    val (show, noshow) = results sortBy (_.sortKey) partition (_.display)
    val padding        = results.head.padding

    println(pp"""
      |Basis sequence was 1 to $max
      |Displaying ${show.size}/${results.size} results - omitted ${noshow.size} less interesting results
      |
      |$padding$banner
      |$padding$underline
      |${show mkString EOL}
      |""".stripMargin)
  }
}
