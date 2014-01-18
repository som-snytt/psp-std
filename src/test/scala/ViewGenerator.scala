package psp
package core
package tests

import psp.common.{ LabeledFunction, LabeledPartialFunction }
import org.specs2.{ mutable => mu }
import scala.util.Random.nextInt
import org.specs2.matcher.ThrownExpectations
import psp.view.ViewOp

class PreciseSpec extends BaseViewSpec("View operations on precise sequences") {
  runCollectionsTests()

  def max    = 1000
  def numOps = 3
  def basisOps: List[Op] = List(
    drop(5),
    dropRight(11),
    slice(7, 41),
    take(13),
    takeRight(17),
    flatMap(nameFn("**2")(x => ((x, x)))),
    filter(nameFn("isEven")(_ % 2 == 0)),
    map(nameFn("*3")(_ * 3))
  )

  def collections  = List[(String, Viewable[Int])](
    "Psp/L"  -> Foreach.to(1, max).psp,
    "Psp/I"  -> Indexed.to(1, max).psp,
    "List"   -> ScalaNative(intList),
    "List/V" -> ScalaNative(intList.view),
    "Stream" -> ScalaNative(intStream),
    "Strm/V" -> ScalaNative(intStream.view),
    "Rnge/V" -> ScalaNative(intRange.view),
    "Vect/V" -> ScalaNative(intVector.view)
  )
}


trait ViewGenerator[A] {
  type ViewOp[A, B] = Viewable[A] => Viewable[B]
  type Op             = ViewOp[A, A]
  type Map[B]        = ViewOp[A, B]
  def MaxOpCount      = 99999

  case class OpCount(view: Viewable[_], result: String, ops: Int)

  def countOps[A, B](xs: Viewable[A])(f: ViewOp[A, B]): OpCount = {
    var ops = 0
    val ys = xs map { x =>
      if (ops >= MaxOpCount)
        return OpCount(null, "<error>", MaxOpCount)
      else
        try x finally ops += 1
    }
    val view   = f(ys)
    val result = view mk_s ", "
    OpCount(view, result, ops)
  }

  def nameFn[R](label: String)(f: A => R)                           = new LabeledFunction(f, label)
  def namePf[R](label: String)(pf: A =?> R)                         = new LabeledPartialFunction(pf, label)
  private def op[B](opLabel: String)(f: ViewOp[A, B]): ViewOp[A, B] = new LabeledFunction(f, opLabel)

  def slice(start: Int, end: Int): Op        = op(ss"slice $start/$end")(_ slice (start, end))
  def take(n: Int): Op                       = op(ss"take $n")(_ take n)
  def drop(n: Int): Op                       = op(ss"drop $n")(_ drop n)
  def filter(p: A => Boolean): Op            = op(ss"filter $p")(_ filter p)
  def withFilter(p: A => Boolean): Op        = op(ss"wFilter $p")(_ filter p)
  def map[B](f: A => B): Map[B]              = op(ss"map $f")(_ map f)
  def flatMap[B](f: A => Foreach[B]): Map[B] = op(ss"flatMap $f")(_ flatMap f)
  def collect[B](pf: A =?> B): Map[B]        = op(ss"collect $pf")(_ filter pf.isDefinedAt map pf)
  def takeRight(n: Int): Op                  = op(ss"takeR $n")(_ takeRight n)
  def dropRight(n: Int): Op                  = op(ss"dropR $n")(_ dropRight n)
  // def takeWhile(p: A => Boolean): Op         = op(ss"takeW $p")(_ takeWhile p)
  // def dropWhile(p: A => Boolean): Op         = op(ss"dropW $p")(_ dropWhile p)
}

trait IntViewGenerator extends ViewGenerator[Int] {
  def seeds = List(30, 60)
}

trait BaseViewSpec0 extends PspSpec with IntViewGenerator {
  def max: Int
  def numOps: Int
  def basisOps: List[Op]
  def collections: List[(String, Viewable[Int])]
  def testOutcomes: List[(Int, String)]

  def intRange  = 1 to max
  def intList   = intRange.toList
  def intVector = intRange.toVector
  def intStream = Stream from 1 take max

  def nonzeroInt(min: Int, max: Int): Int = {
    val nums = min to max filterNot (_ == 0)
    nums(nextInt(nums.length))
  }
  def positiveInt(max: Int): Int = nextInt(max) + 1

  def opsInString(ops: Op*): (Int, List[Int], String) = {
    val op                            = ops :+ take(3) reduceLeft (_ andThen _)
    val answers                       = collections map (_._2) map (countOps(_)(op))
    val OpCount(view, result, pspOps) = answers.head
    val allOps                        = answers map (_.ops)

    val results = answers.map(_.result).distinct filterNot (_ == "<error>") match {
      case r :: Nil => r
      case rs       =>
        val grouped = answers.zipWithIndex.groupBy(_._1.result).mapValues(_ map (_._2) mkString "/").map({ case (k, v) => ss"$v: $k" })
        "!!!!!  " + grouped.mkString(" / ")
    }

    (pspOps, allOps, results)
  }
  def opFmt(s: String) = {
    val args = (s split "\\s+").toList match {
      case x :: Nil => x :: "" :: Nil
      case xs       => xs
    }
    "%8s %8s".format(args: _*)
  }
  def headerPad = (19 * numOps) + 4
  def headerFmt = "%s%-" + headerPad + "s%s"

  def header(): List[String] = {
    val styles = collections map { case (name, _) => "%6s" format name }
    val dashes = styles map (s => "-" * s.length)
    val fmt    = styles map (s => "%" + s.length + "s") mkString "  "
    val line1  = fmt.format(styles: _*)
    val line2  = fmt.format(dashes: _*)
    line1 :: line2 :: Nil
  }

  def runCollectionsTests() = {
    headerFmt.format("", s"Basis collection is 1 to $max", header.head) in true
    headerFmt.format(".", "", header.last) in true
    testOutcomes.sorted foreach { case (_, str) => str in true }
  }
}
abstract class BaseViewSpec(val description: String) extends tests.BaseViewSpec0 {
  def applyOpSequence(ops: Seq[Op]): Option[(Int, String)] = {
    val (count, allCounts, outcome) = opsInString(ops: _*)
    // All counts are either equal to ours or equal to the size of the list (e.g. eager List)
    val isSkip = false//allCounts.distinct forall (n => (n == count) || (n == max))
    if (isSkip) None else {
      val padOps = ops map (x => opFmt(x.to_s)) mk_s ", "
      val padCounts = allCounts map ("%6s" format _) mk_s "  "
      Some(count -> ss".$padOps     $padCounts   // $outcome")
    }
  }

  def nBases(n: Int): List[List[Op]] = {
    val xs = (basisOps combinations n flatMap (_.permutations.toList)).toList.distinct
    val ys = basisOps map (op => List.fill(n)(op)) // each op repeated N times
    xs ++ ys
  }

  def testOutcomes = nBases(numOps) flatMap applyOpSequence
}
