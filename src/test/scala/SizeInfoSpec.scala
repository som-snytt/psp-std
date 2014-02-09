package psp
package tests

import core._
import org.scalacheck._
import SizeInfo._
import SizeInfoGenerators._

abstract class PspSpec extends org.specs2.mutable.Specification with org.specs2.matcher.ThrownExpectations {
  sequential
}

object SizeInfoGenerators {
  import Gen._

  type Matcher[-T]     = org.specs2.matcher.Matcher[T]
  type MatchResult[+T] = org.specs2.matcher.MatchResult[T]
  type ScalaCheck      = org.specs2.ScalaCheck

  implicit class GenOps[A](gen: Gen[A]) {
    def collect[B](pf: A =?> B): Gen[B] = gen suchThat pf.isDefinedAt map pf.apply
  }

  def genSize: Gen[Size]         = chooseNum(1, Int.MaxValue / 2) map (n => Size(n))
  def genPrecise: Gen[Precise]   = genSize map (s => Precise(s))
  def genBounded: Gen[Bounded]   = genSize flatMap (lo => genAtomic map (hi => bounded(lo, hi))) collect { case b: Bounded => b }
  def genAtomic: Gen[Atomic]     = frequency(10 -> genPrecise, 1 -> Empty, 1 -> Infinite)
  def genSizeInfo: Gen[SizeInfo] = oneOf(genAtomic, genBounded)

  trait Pri0              { implicit def arbSize: Arbitrary[Size]         = Arbitrary(genSize) }
  trait Pri1 extends Pri0 { implicit def arbSizeInfo: Arbitrary[SizeInfo] = Arbitrary(genSizeInfo) }
  trait Pri2 extends Pri1 { implicit def arbAtomic: Arbitrary[Atomic]     = Arbitrary(genAtomic) }
  trait Pri3 extends Pri2 { implicit def arbPrecise: Arbitrary[Precise]     = Arbitrary(genPrecise) }
}

class SizeInfoSpec extends PspSpec with ScalaCheck with SizeInfoGenerators.Pri3 {

  type SI = SizeInfo
  type BinOp[T] = (T, T) => T
  type Tried[T] = Either[Throwable, T]

  def beCertain: Matcher[ThreeValue] = be_===(ThreeValue.True)

  private def tried[T](op: => T) = try Right(op) catch { case t: Throwable => Left(t) }

  private def equalOrSameException[T](p1: Tried[T], p2: Tried[T]): Boolean = (p1, p2) match {
    case (Right(x1), Right(x2)) => x1 == x2
    case (Left(t1), Left(t2))   => t1.getClass == t2.getClass
    case _                      => false
  }
  private def beEqualOrThrowSame[T](x: Tried[T]) = beTypedEqualTo(x, equalOrSameException[T])

  // When testing e.g. associativity and the sum overflows, we
  // need to do more than compare values for equality.
  def sameOutcome[T](op1: => T, op2: => T): MatchResult[Tried[T]] = tried(op1) must beEqualOrThrowSame(tried(op2))

  def commutative[T: Arbitrary](op: BinOp[T]): Prop = prop((p1: T, p2: T) => sameOutcome(op(p1, p2), op(p2, p1)))
  def associative[T: Arbitrary](op: BinOp[T]): Prop = prop((p1: T, p2: T, p3: T) => sameOutcome(op(op(p1, p2), p3), op(p1, op(p2, p3))))

  def certain[T: Arbitrary](f: T => ThreeValue): Prop                    = prop((p1: T) => f(p1) must beCertain)
  def certain[T: Arbitrary, U: Arbitrary](f: (T, U) => ThreeValue): Prop = prop((p1: T, p2: U) => f(p1, p2) must beCertain)

  def flip(r: Prop.Result): Prop.Result = r match {
    case Prop.Result(Prop.True, _, _, _)  => r.copy(status = Prop.False)
    case Prop.Result(Prop.False, _, _, _) => r.copy(status = Prop.True)
    case _                                => r
  }

    "`+` on precises"   ! prop((s: Precise, n: Size) => (s + n).size must_== s.size + n)
    "s1 <= (s1 max s2)" ! certain((s1: Atomic, s2: Atomic) => s1 <= (s1 max s2))
    "s1 >= (s1 min s2)" ! certain((s1: Atomic, s2: Atomic) => s1 >= (s1 min s2))
      "s1 <= (s1 + s2)" ! certain((s1: Atomic, s2: Atomic) => s1 <= (s1 + s2))
      "s1 >= (s1 - s2)" ! certain((s1: Atomic, s2: Precise) => s1 >= (s1 - s2))
            "<inf> + n" ! certain((s1: SI) => (Infinite + s1) <==> Infinite)

     "`+` is associative" ! associative[SI](_ + _)
   "`max` is associative" ! associative[SI](_ max _)
   "`min` is associative" ! associative[SI](_ min _)
     "`+` is commutative" ! commutative[SI](_ + _)
   "`max` is commutative" ! commutative[SI](_ max _)
   "`min` is commutative" ! commutative[SI](_ min _)

 // "`-` is NOT associative" ! (associative[SizeInfo](_ - _) map flip)
}
