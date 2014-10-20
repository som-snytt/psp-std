package psp
package tests

import compat.ScalaNative
import psp.std._, api._, StdShow._, StdEq._

object IntViews {
  type IntView    = View[Int]
  type IntViewFun = IntView => IntView
  type IntPred    = Predicate[Int]

  // Type inference buddies.
  private def fn[A](f: Int => A): Int => A    = f
  private def pfn[A](f: Int ?=> A): Int ?=> A = f

  lazy val tupleFlatMap: Int => pSeq[Int] = "(x, x)" |: fn(x => fromElems(x, x))
  lazy val isEven: IntPred                = "isEven" |: divisibleBy(2)
  lazy val timesThree: Int => Int         = "*3"     |: fn(_ * 3)
  lazy val collectDivSix: Int ?=> Int     = "%/6"    |: pfn({ case x if x % 6 == 0 => x / 6 })
  lazy val isOdd: IntPred                 = "isOdd"  |: !isEven

  def divisibleBy(n: Int): IntPred            = s"/$n"             |: fn(_ % n == 0)
  def lessThan(n: Int): IntPred               = s"<$n"             |: fn(_ < n)
  def fizzbuzz(fizz: Int, buzz: Int): IntPred = s"fb[$fizz/$buzz]" |: !(divisibleBy(fizz) ^ divisibleBy(buzz))
}
import IntViews._

class OperationCounts(scalaVersion: String) extends Bundle {
  def bundle = "Operation Counts"

  def is211 = scalaVersion == "2.11"
  def run(): Boolean = {
    results foreach (r => assert(r.isAgreement, r))
    showResults()
    finish()
  }
  def max    = 500
  def numOps = 3
  def basicOps = if (is211) basicOps211 else basicOps210

  private def basicOps210 = List[IntViewFun](
    _ drop 5,
    _ slice indexRange(7, 41) labeledAs "slice(7,41)",
    _ take 13,
    _ flatMap tupleFlatMap,
    _ filter isEven,
    _ map timesThree,
    _ collect collectDivSix,
    _ takeWhile lessThan(max / 3),
    _ dropWhile lessThan(max / 3)
  )
  /** Can't use dropRight and takeRight in 2.10 without the scala library
   *  implementations going off the rails entirely.
   */
  private def basicOps211 = basicOps210 ++ List[IntViewFun](
    _ dropRight 11,
    _ takeRight 17
  )

  def policyList   = (1 to max).toPolicyList
  def policyVector = (1 to max).pvec
  def policyMixed  = (1 until max / 2).pvec.m ++ (max / 2 to max).toPolicyList.m

  def scalaIntRange: sciRange = sciRange.inclusive(1, max, 1)
  def scalaIntList            = scalaIntRange.toList
  def scalaIntStream          = scalaIntRange.toStream
  def scalaIntVector          = scalaIntRange.toVector

    // (Foreach from 1 take max).m,
  def usCollections = newMap[String, IntView](
    "plist"      -> policyList.m,
    "plist+size" -> (policyList.m sized newSize(max)),
    "pvec"       -> policyVector.m,
    "pmixed"     -> policyMixed
  )
  def themCollections = newMap[String, IntView](
    "slist"        -> ScalaNative(scalaIntList.view),
    "sstream"      -> ScalaNative(scalaIntStream),
    "sstream.view" -> ScalaNative(scalaIntStream.view),
    "srange.view"  -> ScalaNative(scalaIntRange.view),
    "svector.view" -> ScalaNative(scalaIntVector.view)
  )
  def rootCollections: pMap[String, IntView] = usCollections ++ themCollections

  def compositesOfN(n: Int): List[IntViewFun] = (
    (basicOps combinations n flatMap (_.permutations.toList)).toList.distinct
      map (xss => xss reduceLeft (_ andThen _))
  )

  class CollectionResult(viewFn: IntViewFun, xs: IntView) {
    val view    = viewFn(xs)
    val result  = view take 3 mkString ", "
    val count   = xs.calls
    def display = "<xs>  " + (view.viewChain.pvec.reverse map (v => fmtdesc(v.description)) filterNot (_.trim.length == 0)).join(" ")

    def fmtdesc(description: String): String = description indexOf ' ' match {
      case -1  => "%-15s" format description
      case idx => "%-7s %-7s".format(description.substring(0, idx), description.substring(idx + 1))
    }
    override def toString = display
  }

  class CompositeOp(viewFn: IntViewFun) {
    val control: CollectionResult       = new CollectionResult(viewFn, ScalaNative(scalaIntList))
    val us: pVector[CollectionResult]   = usCollections.values map (xs => new CollectionResult(viewFn, xs))
    val them: pVector[CollectionResult] = themCollections.values map (xs => new CollectionResult(viewFn, xs))
    val all: pVector[CollectionResult]  = us ++ newVector(control) ++ them

    def usCounts   = us map (_.count)
    def themCounts = them map (_.count)
    def allResults = all map (_.result)
    def allCounts  = all map (_.count)

    def usAverage   = usCounts.sum / us.sizeInfo.toDouble
    def themAverage = themCounts.sum / them.sizeInfo.toDouble
    def ratioDouble = themAverage / usAverage
    def ratio       = if (ratioDouble == PositiveInfinity) "Inf" else "%.2f" format ratioDouble

    def headResult  = us.head

    def fmtdesc(description: String): String = description indexOf ' ' match {
      case -1  => "%-15s" format description
      case idx => "%-7s %-7s".format(description.substring(0, idx), description.substring(idx + 1))
    }
    def headView      = headResult.toString
    def isAgreement   = allResults.distinct.sizeInfo == 1.size
    def display       = (
         isTestDebug
      || !isAgreement
      || usCounts.distinct.sizeInfo == usCollections.sizeInfo
      || ratio == "Inf"
      || ratioDouble < 1   // horrors!
    )
    def countsString  = allCounts map ("%7s" format _) mkString " "
    def resultsString = if (isAgreement) headResult.result else "!!! " + failedString
    def failedString   = {
      val grouped = all.toScalaVector.zipWithIndex groupBy { case (x, i) => x.result }
      val lines = grouped.toList map { case (res, pairs) => "%20s:  %s".format(pairs.map(_._2).mkString(" "), res) }
      lines.mkString("\n  ", "\n  ", "\n")
    }
    def padding        = " " * (headView.length + 2)
    def sortKey        = ((-ratioDouble, usCounts.min))

    override def toString = "%s  %6s %s  //  %s".format(headView, ratio, countsString, resultsString)
  }

  lazy val results = compositesOfN(numOps) map (fn => new CompositeOp(fn))

  private def showResults() {
    val (show, noshow)    = results partition (_.display)
    val banner: String    = List("Improve", "Linear", "Sized", "Direct", "50/50", "<EAGER>", "ListV", "Stream", "StreamV", "RangeV", "VectorV") map ("%7s" format _) mkString " "
    val underline: String = banner.toCharArray.m map (ch => if (ch == ' ') ' ' else '-') mkString ""
    def padding = show.head.padding

    if (show.isEmpty) {
      if (is211) {
        results foreach println
        abort("Something is wrong if we never see a line where each of our view types has a different count")
      }
      else println(s"\nNo especially interesting operation counts amongst ${results.size} results.")
    }
    else println("\n" + pp"""
      |Displaying ${show.size}/${results.size} results on basis sequence 1 to $max (omitted ${noshow.size})
      |
      |$padding$banner
      |$padding$underline
      |${show mkString EOL}
      |""".stripMargin)
  }
}
