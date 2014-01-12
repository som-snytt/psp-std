package psp
package core
package tests

// Blanket import of this package causes CCE as described in https://issues.scala-lang.org/browse/SI-8133
// import psp.common._
//
// Can also be reproduced via
// import psp.common.`package`
//
import psp.common.{ LabeledFunction, LabeledPartialFunction }
import org.specs2.{ mutable => mu }
import scala.util.Random.nextInt
import org.specs2.matcher.ThrownExpectations

class InfiniteSpec extends BaseViewSpec("View operations on infinite sequences") {
  runCollectionsTests()

  def max          = 100
  def runs         = 20
  def operationFns = infiniteSpecOps
  def opSeeds      = List(5, 10, 15)
  def collections  = List[(String, Viewable[Int])](
    "Psp"    -> PspView(Foreach from 1),
    "Stream" -> ScalaNative(Stream from 1),
    "Strm/V" -> ScalaNative(Stream from 1 view)
  )
}
class PreciseSpec extends BaseViewSpec("View operations on precise sequences") {
  runCollectionsTests()

  def max          = 100
  def runs         = 50
  def operationFns = preciseSpecOps
  def opSeeds      = List(25, 50, 75)
  def collections  = List[(String, Viewable[Int])](
    "Psp/L"  -> (PspView(Foreach from 1) take max),
    "Psp/I"  -> PspView(Indexed.to(1, max)),
    "List"   -> ScalaNative(intList),
    "List/V" -> ScalaNative(intList.view),
    "Stream" -> ScalaNative(intStream),
    "Strm/V" -> ScalaNative(intStream.view),
    "Rnge/V" -> ScalaNative(intRange.view),
    "Vect/V" -> ScalaNative(intVector.view)
  )
}


trait ViewGenerator[A] {
  type ViewOp[-A, +B] = Viewable[A] => Viewable[B]
  type Op             = ViewOp[A, A]
  type Map[+B]        = ViewOp[A, B]

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
    val result = view mkString ", "
    OpCount(view, result, ops)
  }

  def nameFn[R](label: String)(f: A => R)                           = new LabeledFunction(f, label)
  def namePf[R](label: String)(pf: A =?> R)                         = new LabeledPartialFunction(pf, label)
  private def op[B](opLabel: String)(f: ViewOp[A, B]): ViewOp[A, B] = new LabeledFunction(f, opLabel)

  def slice(start: Int, end: Int): Op        = op(ss"slice $start/$end")(_ slice (start, end))
  def take(n: Int): Op                       = op(ss"take $n")(_ take n)
  def drop(n: Int): Op                       = op(ss"drop $n")(_ drop n)
  def takeRight(n: Int): Op                  = op(ss"takeR $n")(_ takeRight n)
  def dropRight(n: Int): Op                  = op(ss"dropR $n")(_ dropRight n)
  def takeWhile(p: A => Boolean): Op         = op(ss"takeW $p")(_ takeWhile p)
  def filter(p: A => Boolean): Op            = op(ss"filter $p")(_ filter p)
  def withFilter(p: A => Boolean): Op        = op(ss"wFilter $p")(_ filter p)
  def dropWhile(p: A => Boolean): Op         = op(ss"dropW $p")(_ dropWhile p)
  def map[B](f: A => B): Map[B]              = op(ss"map $f")(_ map f)
  def flatMap[B](f: A => Foreach[B]): Map[B] = op(ss"flatMap $f")(_ flatMap f)
  def collect[B](pf: A =?> B): Map[B]        = op(ss"collect $pf")(_ collect pf)
}

trait IntViewGenerator extends ViewGenerator[Int] {
  def safeOps: Array[Int => Op] = Array(
    take,
    drop,
    slice(nextInt(5), _),
    dropRight,
    n => filter(nameFn(ss"/$n")(_ % n == 0)),
    n => map(nameFn(ss"*$n")(_ * n)),
    n => flatMap(nameFn(ss"**${ math abs n }")(x => Indexed.repeat(math abs n, x))),
    n => collect(namePf(ss"$n**") { case x if x % n == 0 => x / 3 })
  )
    // n => withFilter(nameFn(ss"/$n")(_ % n == 0)),
  def unsafeOps: Array[Int => Op] = Array(
    takeRight,
    n => takeWhile(nameFn(ss"<$n")(_ < n)),
    n => dropWhile(nameFn(ss"<$n")(_ < n))
  )

  def preciseSpecOps   = safeOps ++ unsafeOps
  def infiniteSpecOps = safeOps
}

trait BaseViewSpec0 extends PspSpec with IntViewGenerator {
  def max: Int
  def runs: Int
  def opSeeds: List[Int]
  def operationFns: Array[Int => Op]
  def collections: List[(String, Viewable[Int])]
  def testOutcomes: List[(Int, String)]

  def numOps       = opSeeds.size
  def intRange     = 1 to max
  def intList      = intRange.toList
  def intVector    = intRange.toVector
  def intStream    = Stream from 1 take max

  def nonzeroInt(min: Int, max: Int): Int = {
    val nums = min to max filterNot (_ == 0)
    nums(nextInt(nums.length))
  }
  def positiveInt(max: Int): Int = nextInt(max) + 1

  def opsInString(ops: Op*): (Int, List[Int], String) = {
    val op                            = ops :+ take(5) reduceLeft (_ andThen _)
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

  def rOperation(max: Int): Op = {
    val x1 = nextInt(operationFns.length)
    val x2 = positiveInt(max)
    operationFns(x1)(x2)
  }
  def opFmt(s: String) = {
    val args = (s split "\\s+").toList match {
      case x :: Nil => x :: "" :: Nil
      case xs       => xs
    }
    "%8s %4s".format(args: _*)
  }
  def headerPad = (15 * numOps) + 4
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
    val isSkip = allCounts.distinct forall (n => (n == count) || (n == max))
    if (isSkip) None else {
      val padOps = ops map (x => opFmt(x.to_s)) mk_s ", "
      val padCounts = allCounts map ("%6s" format _) mk_s "  "
      Some(count -> ss".$padOps     $padCounts   // $outcome")
    }
  }
  def testOutcomes = List.fill(runs)(opSeeds map rOperation).distinct flatMap applyOpSequence
}
