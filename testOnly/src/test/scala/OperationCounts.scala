package psp
package tests

import compat.ScalaNative
import psp.std._, api._, StdShow._, StdEq._
import org.scalacheck._

object IntViews {
  type IntView    = View[Int]
  type IntViewFun = IntView => IntView
  type IntPred    = Predicate[Int]

  // Type inference buddies.
  def fn[A](f: Int => A): Int => A      = f
  def pfn[A](f: Int ?=> A): Int ?=> A   = f
  def vfn[A](f: IntViewFun): IntViewFun = f

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

class CollectionResult(viewFn: IntViewFun, nxs: NamedView) {
  val name        = nxs.name
  val xs          = nxs.view
  val description = xs.description
  val size        = xs.size
  val applied     = viewFn(xs)
  val front3      = (applied take 3).force.pvec
  val result      = front3.to_s
  val count       = xs.calls
  override def toString = show"$name input: $xs operation: $description size: $size result: $result count: $count"
}

final case class NamedView(name: String, view: IntView) extends NaturalHashEq {
  override def toString = show"$name: $view"
}

class OperationCounts(scalaVersion: String) extends ScalacheckBundle {
  def bundle      = "Operation Counts"
  def max         = 100
  def numOps      = 3
  def collections = Direct[NamedView](
    NamedView("p/linear", policyList.m),
    NamedView("p/sized", policyList.m sized newSize(max)),
    NamedView("p/direct", policyVector.m),
    NamedView("p/mixed1", policyMixed1.m),
    NamedView("s/listv", ScalaNative(scalaIntList.view)),
    NamedView("s/stream", ScalaNative(scalaIntStream)),
    NamedView("s/streamv", ScalaNative(scalaIntStream.view)),
    NamedView("s/vectorv", ScalaNative(scalaIntVector.view))
  )
  def control = NamedView("EAGER", ScalaNative(scalaIntList))
  def basicOps = if (scalaVersion == "2.11") basicOps211 else basicOps210

  private def basicOps210 = List[IntViewFun](
    _ collect collectDivSix,
    _ drop 5,
    _ dropWhile lessThan(max / 3),
    _ filter isEven,
    _ flatMap tupleFlatMap,
    _ map timesThree,
    _ slice indexRange(7, 41),
    _ take 13,
    _ takeWhile lessThan(max / 3)
  )
  /** Can't use dropRight and takeRight in 2.10 without the scala library
   *  implementations going off the rails entirely.
   */
  private def basicOps211 = basicOps210 ++ List[IntViewFun](
    _ dropRight 11,
    _ takeRight 17
  )

  // TODO
  // _ dropIndex 2.index,
  // Index(max / 2) |> (i => vfn(_ splitAt i left) :| "splitAt($i).left"),
  // Index(max / 3) |> (i => vfn(_ splitAt i right) :| "splitAt($i).right"),
  // (max / 4)      |> (n => vfn(_ span lessThan(n) left) :| "span (_ < $n) left"),
  // (max / 5)      |> (n => vfn(_ span lessThan(n) right) :| "span (_ < $n) right"),

  def policyList: pList[Int]         = (1 to max).plist
  def policyVector: pVector[Int]     = (1 to max).pvec
  def policyMixed1: pSeq[Int]        = Foreach.join((1 until max / 2).pvec, (max / 2 to max).plist)
  def policyMixed2: pSeq[Int]        = Foreach.join((1 until max / 2).plist, (max / 2 to max).pvec)
  def scalaIntRange: sciRange        = sciRange.inclusive(1, max, 1)
  def scalaIntList: sciList[Int]     = scalaIntRange.toList
  def scalaIntStream: sciStream[Int] = scalaIntRange.toStream
  def scalaIntVector: sciVector[Int] = scalaIntRange.toVector

  def compositesOfN(n: Int): List[IntViewFun] = (
    (basicOps combinations n flatMap (_.permutations.toList)).toList.distinct
      map (xss => xss reduceLeft (_ andThen _))
  )
  class CompositeOp(viewFn: IntViewFun) {
    val eager          = NamedView("EAGER", ScalaNative(scalaIntList))
    val eagerOutcome   = new CollectionResult(viewFn, eager)
    val expected       = eagerOutcome.result
    val indices        = collections.indices
    val outcomes       = collections map (xs => new CollectionResult(viewFn, xs))
    val counts         = outcomes map (r => "%-3s".format(r.count)) mkString " "
    val description    = "%s  %s  // %s".format(viewFn(Direct[Int]().m).chainDescriptions.filterNot(_ == "<xs>").map("%-15s" format _).joinWords.render, counts, expected)
    def passed         = outcomes.map(_.result).distinct.size == 1.size
    def distinctCounts = outcomes.map(_.count).distinct
    def isInteresting  = distinctCounts.size >= 4.size

    override def toString = (
      description.asis <@> indices.tabular(
        i => collections(i).name,
        i => outcomes(i).count.to_s,
        i => (outcomes(i).result |> (r => if (r == expected) "" else s"  // !!! found: $r"))
      ).asis render
    )

  }

  lazy val results = compositesOfN(numOps) map (fn => new CompositeOp(fn)) filter (_.isInteresting)

  def props(): Seq[NamedProp] = results map (r => NamedProp(r.description, r.passed))
}
