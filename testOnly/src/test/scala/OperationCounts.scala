package psp
package tests

import psp.std._, api._, StdShow._, StdEq._
import lowlevel.ExclusiveIntRange
import scala.{ collection => sc }
import sc.{ mutable => scm, immutable => sci }

class OperationCounts extends ScalacheckBundle {
  type IntView    = TestedViewMethods
  type IntViewFun = Unary[IntView]

  def vfn[A](f: IntViewFun): IntViewFun = f

  final case class CompositeFun(fs: Direct[IntViewFun]) extends ForceShowDirect {
    val to_s = "%-47s" format (fs map ("%-15s" format _.try_s) mk_s " ")
    val f: IntViewFun = fs reducel (_ andThen _)

    def apply(xs: IntView): IntView = f(xs)
  }
  class CollectionResult(viewFn: CompositeFun, testedFn: RecorderCounter => TestedViewMethods) extends ForceShowDirect {
    val counter = new RecorderCounter
    val xs      = testedFn(counter)
    val name    = xs.name
    val applied = viewFn(xs) take 3
    val result  = applied.to_s
    val calls   = counter.distinctCalls
    val hits    = counter.totalCalls

    def counted = if (isTestDebug) countedDebug else "%3s".format(calls)
    def countedDebug = "%3s/%-4s".format(calls, if (hits == calls) "" else hits)
    def to_s    = fshow"$name%12s  $result%s"
  }

  def bundle      = "Operation Counts"
  def max         = 100
  def half        = max / 2
  def third       = max / 3
  def numOps      = 3
  def collections = Direct[RecorderCounter => TestedViewMethods](
    c => TestPolicyView("p/linear", Probe.Linear(1 to max, c).view),
    c => TestScalaView("s/linear",  Probe.ScalaLinear(1 to max, c).view),
    c => TestPolicyView("p/direct", Probe.Direct(1 to max, c).view),
    c => TestScalaView("s/direct",  Probe.ScalaDirect(1 to max, c).view)
  )
  // c => TestScalaView("s/stream",  Stream from 1 take 100 map c.record),
  // c => TestScalaView("s/streamv", (Stream from 1 take 100).view map c.record),
  // c => TestPolicyView("p/sized",  new LinearView(Probe.Sized(1 to max, c))),
  // c => TestPolicyView("p/mixed1", new DirectView(Probe.Direct(1 until half, c)) ++ new LinearView(Probe.Linear(half to max, c))),

  private def basicOps = sciList[IntViewFun](
    "%/2"            |: vfn(_ collect { case x if x % 2 == 0 => x / 2 }),
    "drop 5"         |: vfn(_ drop 5),
    "drop 25"        |: vfn(_ drop 25),
    s"dropW <$third" |: vfn(_ dropWhile (_ < third)),
    "isEven"         |: vfn(_ withFilter (_ % 2 == 0)),
    "x=>(x, x)"      |: vfn(_ flatMap (x => Direct(x, x))),
    "map *3"         |: vfn(_ map (_ * 3)),
    "take 13"        |: vfn(_ take 13),
    "take 42"        |: vfn(_ take 42),
    s"takeW <$third" |: vfn(_ takeWhile (_ < third)),
    "dropR 11"       |: vfn(_ dropRight 11),
    "takeR 17"       |: vfn(_ takeRight 17)
  )

  def compositesOfN(n: Int): Direct[CompositeFun] = (
    (basicOps combinations n flatMap (_.permutations)).toList.m.pvec.map(_.m.pvec).distinctByEquals map CompositeFun
  )
  class CompositeOp(viewFn: CompositeFun) {
    val outcomes  = collections map (collectionFn => new CollectionResult(viewFn, collectionFn)) pvec
    val Each(usLinear, themLinear, usDirect, themDirect) = outcomes

    def compare(lhs: Int, rhs: Int): String = "%3s %-2s %-3s".format(lhs, if (lhs <= rhs) "<=" else ">", rhs)

    val counts    = "%s  %s".format(compare(usLinear.calls, themLinear.calls), compare(usDirect.calls, themDirect.calls))
    val results   = outcomes map (_.result) pvec

    def description    = if (passed) show"$viewFn  $counts  // ${results.head}" else show"Inconsistent results for $viewFn:\n  ${outcomes mk_s "\n  "}"
    def passed         = results.distinct.size == 1.size && (usLinear.calls <= themLinear.calls) && (usDirect.calls <= themDirect.calls)
    def distinctCounts = outcomes.map(_.calls).distinct.pvec
    def isInteresting  = !passed || distinctCounts.size >= 3.size || isTestDebug
  }

  lazy val results = Direct(1, 2, 3) flatMap compositesOfN map (fn => new CompositeOp(fn)) filter (_.isInteresting)

  def props(): sciList[NamedProp] = {
    NamedProp("%-49s %-12s %s".format("", "Linear", "Direct"), Prop(true)) :: (
      results.toScalaList map (r => NamedProp(r.description, r.passed))
    )
  }
}
