package psp
package tests

import psp.std._, api._, StdShow._, StdEq._
import org.scalacheck._, Prop.forAll, Gen._
import lowlevel.ExclusiveIntRange
import scala.{ collection => sc }
import sc.{ mutable => scm, immutable => sci }

class CollectionResult(viewOp: ViewClass.Op, testedFn: RecorderCounter => ViewClass) extends ForceShowDirect {
  val counter = new RecorderCounter
  val xs      = testedFn(counter)
  val name    = xs.name
  val applied = viewOp(xs) take 3
  val result  = applied.to_s
  val calls   = counter.distinctCalls
  val hits    = counter.totalCalls

  def counted = if (isTestDebug) countedDebug else "%3s".format(calls)
  def countedDebug = "%3s/%-4s".format(calls, if (hits == calls) "" else hits)
  def to_s    = fshow"$name%12s  $result%s"
}

class OperationCounts extends ScalacheckBundle {
  private[this] var displaysRemaining = maxDisplay

  def bundle                 = "Operation Counts"
  def max                    = 100
  def minSuccessful: Precise = 1000.size
  def maxDisplay: Precise    = 20.size
  def numComposite           = SizeRange(2, 4)
  def collections            = Direct[RecorderCounter => ViewClass](
    c => PolicyViewClass("p/linear", Probe.Linear(1 to max, c).view),
    c => ScalaViewClass("s/linear",  Probe.ScalaLinear(1 to max, c).view),
    c => PolicyViewClass("p/direct", Probe.Direct(1 to max, c).view),
    c => ScalaViewClass("s/direct",  Probe.ScalaDirect(1 to max, c).view)
  )
  def chooseMax   = choose(0, max)
  def lowHalf     = choose(0, max / 2)
  def highHalf    = choose(max / 2, max)
  def chooseSmall = choose(1, max / 20)

  private def vfn(f: ViewClass.Op): ViewClass.Op = f

  def viewMethod: Gen[ViewClass.Op] = oneOf(
    lowHalf       map (n => s"drop $n"   |: vfn(_ drop n)),
    highHalf      map (n => s"take $n"   |: vfn(_ take n)),
    chooseMax     map (n => s"dropR $n"  |: vfn(_ dropRight n)),
    chooseMax     map (n => s"takeR $n"  |: vfn(_ takeRight n)),
    lowHalf       map (n => s"dropW <$n" |: vfn(_ dropWhile (_ < n))),
    lowHalf       map (n => s"takeW <$n" |: vfn(_ takeWhile (_ < n))),
    chooseSmall   map (n => s"*$n"       |: vfn(_ map (_ * n))),
    chooseSmall   map (n => s"/$n"       |: vfn(_ withFilter (_ % n == 0))),
    chooseSmall   map (n => s"!/$n"      |: vfn(_ filterNot (_ % n == 0))),
    chooseSmall   map (n => s"%/$n"      |: vfn(_ collect { case x if x % n == 0 => x / n })),
    chooseSmall   map (n => s"x=>(x, x)" |: vfn(_ flatMap (x => Direct(x, x)))),
    (lowHalf, chooseMax) map ((n1, n2) => s"slice($n1, $n2)" |: vfn(_ slice indexRange(n1, n2)))
  )

  def viewMethods(size: SizeRange): Gen[Direct[ViewClass.Op]] = size.chooseInt flatMap (n => listOfN(n, viewMethod) map (_.m.pvec))
  def composite: Gen[CompositeOp]                             = viewMethods(numComposite) map (fs => new CompositeOp(fs))
  def compositeProp: Prop                                     = forAll((_: CompositeOp).passed) minSuccessful minSuccessful

  implicit def arbComposite: Arbitrary[CompositeOp] = Arbitrary(composite)


  class CompositeOp(ops: Direct[ViewClass.Op]) {
    lazy val Each(usLinear, themLinear, usDirect, themDirect) = outcomes

    lazy val viewOp: ViewClass.Op               = xs => ops.foldl(xs)((res, f) => f(res))
    lazy val outcomes: Direct[CollectionResult] = collections map (collectionFn => new CollectionResult(viewOp, collectionFn))
    lazy val counts: String                     = "%s  %s".format(compare(usLinear.calls, themLinear.calls), compare(usDirect.calls, themDirect.calls))
    lazy val results: Direct[String]            = outcomes map (_.result)

    lazy val failed = (
         results.distinct.size != 1.size
      || usLinear.calls > themLinear.calls
      || usDirect.calls > themDirect.calls
    )
    lazy val passed = Try(!failed).fold(
      t => andFalse(t.printStackTrace),
      x => try x finally maybeShow(x)
    )

    private def maybeShow(passed: Boolean): Unit = {
      if (!passed)
        println(failString)
      else if (isTestDebug || (displaysRemaining.isPositive && distinctCounts.size >= 3.size))
        try println(passString) finally displaysRemaining -= 1.size
    }

    def compare(lhs: Int, rhs: Int): String = "%3s %-2s %-3s".format(lhs, if (lhs <= rhs) "<=" else ">", rhs)

    def ops_s          = "%-63s" format (ops map ("%-15s" format _.try_s) mk_s " ")
    def description    = if (passed) passString else failString
    def passString     = show"$ops_s  $counts  // ${results.head}"
    def failString     = show"Inconsistent results for $ops_s:\n  ${outcomes mk_s "\n  "}"
    def distinctCounts = outcomes.map(_.calls).distinct.pvec
    def isInteresting  = !passed || distinctCounts.size >= 3.size || isTestDebug
    override def toString = description
  }

  def props() = sciList[NamedProp](
    NamedProp(s"Generating $minSuccessful view combinations, displaying at most $maxDisplay", Prop(true)),
    NamedProp("%-58s %-12s %s".format("", "Linear", "Direct"), Prop(true)),
    NamedProp("policy never performs more operations than scala", compositeProp)
  )
}
