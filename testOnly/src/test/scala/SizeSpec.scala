package psp
package tests

import org.scalacheck._, Prop._, Gen._
import psp.std._, api._
import StdEq._

object SizeSpec {
  def precise: Gen[Precise] = chooseNum(1, MaxInt / 2) map (_.size)
  def atomic: Gen[Atomic]   = frequency(10 -> precise, 1 -> 0.size, 1 -> Infinite)
  def bounded: Gen[Bounded] = precise flatMap (lo => atomic map (hi => api.Size(lo, hi))) collect classFilter[Bounded]
  def size: Gen[Size]       = oneOf(atomic, bounded)
}

trait PspArb1                 { implicit def arbSize: Arb[Size]       = Arb(SizeSpec.size)     }
trait PspArb2 extends PspArb1 { implicit def arbAtomic: Arb[Atomic]   = Arb(SizeSpec.atomic)   }
trait PspArb3 extends PspArb2 { implicit def arbPrecise: Arb[Precise] = Arb(SizeSpec.precise)  }

class SizeSpec extends ScalacheckBundle with PspArb3 {
  def bundle = "Size laws"

  def certain[T: Arb, U: Arb](f: (T, U) => Boolean): Prop = forAll((p1: T, p2: U) => printResultIf(false, s"op($p1, $p2)")(f(p1, p2)))
  def commutative[T: Arb : Eq](op: BinOp[T]): Prop        = forAll((p1: T, p2: T) => printResultIf(false, s"op($p1, $p2)")(sameBehavior(op(p1, p2), op(p2, p1))))
  def associative[T: Arb : Eq](op: BinOp[T]): Prop        = forAll((p1: T, p2: T, p3: T) => printResultIf(false, s"op($p1, $p2, $p3)")(sameBehavior(op(op(p1, p2), p3), op(p1, op(p2, p3)))))
  def associatives[A: Arb : Eq](ops: BinOp[A]*): Prop     = ops map (x => associative(x)) reduceLeft (_ && _)
  def commutatives[A: Arb : Eq](ops: BinOp[A]*): Prop     = ops map (x => commutative(x)) reduceLeft (_ && _)

  def flip(r: Prop.Result): Prop.Result = r match {
    case Prop.Result(Prop.True, _, _, _)  => r.copy(status = Prop.False)
    case Prop.Result(Prop.False, _, _, _) => r.copy(status = Prop.True)
    case _                                => r
  }

  // ...Aaaaand right on cue, a bunch of these tests broke until I added a type annotation.
  def props = sciList[NamedProp](
    "s1 <= (s1 max s2)"  -> certain[Atomic, Atomic]((s1, s2) => (s1: Size) p_<= (s1 max s2)),
    "s1 >= (s1 min s2)"  -> certain[Atomic, Atomic]((s1, s2) => (s1: Size) p_>= (s1 min s2)),
    "s1 <= (s1 + s2)"    -> certain[Atomic, Atomic]((s1, s2) => (s1: Size) p_<= (s1 + s2)),
    "s1 >= (s1 - s2)"    -> certain[Atomic, Precise]((s1, s2) => (s1: Size) p_>= (s1 - s2)),
    "<inf> + n"          -> forAll((s1: Size) => ((Infinite + s1) partialCompare Infinite) == PCmp.EQ),
    "+ is commutative"   -> commutative[Size](_ + _),
    "max is associative" -> associative[Size](_ max _),
    "max is commutative" -> commutative[Size](_ max _),
    "min is associative" -> associative[Size](_ min _),
    "min is commutative" -> commutative[Size](_ min _)
  )
}
