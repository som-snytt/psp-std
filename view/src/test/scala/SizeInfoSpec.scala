package psp
package tests

import utest._
import core._
import org.scalacheck._
import SizeInfo._
import SizeInfoGenerators._
import org.scalacheck.Prop._

object SizeInfoGenerators {
  import Gen._

  implicit class GenOps[A](gen: Gen[A]) {
    def collect[B](pf: A ?=> B): Gen[B] = gen suchThat pf.isDefinedAt map pf.apply
  }

  def genSize: Gen[Size]         = chooseNum(1, Int.MaxValue / 2) map (n => Size(n))
  def genPrecise: Gen[Precise]   = genSize map (s => Precise(s))
  def genBounded: Gen[Bounded]   = genSize flatMap (lo => genAtomic map (hi => bounded(lo, hi))) collect { case b: Bounded => b }
  def genAtomic: Gen[Atomic]     = frequency(10 -> genPrecise, 1 -> Empty, 1 -> Infinite)
  def genSizeInfo: Gen[SizeInfo] = oneOf(genAtomic, genBounded)

  trait Pri0              { implicit def arbSize: Arbitrary[Size]         = Arbitrary(genSize) }
  trait Pri1 extends Pri0 { implicit def arbSizeInfo: Arbitrary[SizeInfo] = Arbitrary(genSizeInfo) }
  trait Pri2 extends Pri1 { implicit def arbAtomic: Arbitrary[Atomic]     = Arbitrary(genAtomic) }
  trait Pri3 extends Pri2 { implicit def arbPrecise: Arbitrary[Precise]   = Arbitrary(genPrecise) }
}

object SizeInfoSpec extends Properties("SizeInfo") with SizeInfoGenerators.Pri3 {
  type SI = SizeInfo
  type BinOp[T] = (T, T) => T
  type Tried[T] = Either[Throwable, T]

  private def tried[T](op: => T) = try Right(op) catch { case t: Throwable => Left(t) }

  // When testing e.g. associativity and the sum overflows, we
  // need to do more than compare values for equality.
  private def sameOutcome[T](p1: => T, p2: => T): Boolean = (tried(p1), tried(p2)) match {
    case (Right(x1), Right(x2)) => x1 == x2
    case (Left(t1), Left(t2))   => t1.getClass == t2.getClass
    case _                      => false
  }

  def commutative[T: Arbitrary](op: BinOp[T]): Prop = forAll((p1: T, p2: T) => sameOutcome(op(p1, p2), op(p2, p1)))
  def associative[T: Arbitrary](op: BinOp[T]): Prop = forAll((p1: T, p2: T, p3: T) => sameOutcome(op(op(p1, p2), p3), op(p1, op(p2, p3))))

  def certain[T: Arbitrary](f: T => ThreeValue): Prop                    = forAll((p1: T) => f(p1).isTrue)
  def certain[T: Arbitrary, U: Arbitrary](f: (T, U) => ThreeValue): Prop = forAll((p1: T, p2: U) => f(p1, p2).isTrue)

  def flip(r: Prop.Result): Prop.Result = r match {
    case Prop.Result(Prop.True, _, _, _)  => r.copy(status = Prop.False)
    case Prop.Result(Prop.False, _, _, _) => r.copy(status = Prop.True)
    case _                                => r
  }

  property("`+` on precises")      = forAll((s: Precise, n: Size) => (s + n).size == s.size + n)
  property("s1 <= (s1 max s2)")    = certain((s1: Atomic, s2: Atomic) => s1 <= (s1 max s2))
  property("s1 >= (s1 min s2)")    = certain((s1: Atomic, s2: Atomic) => s1 >= (s1 min s2))
  property("s1 <= (s1 + s2)")      = certain((s1: Atomic, s2: Atomic) => s1 <= (s1 + s2))
  property("s1 >= (s1 - s2)")      = certain((s1: Atomic, s2: Precise) => s1 >= (s1 - s2))
  property("<inf> + n")            = certain((s1: SI) => (Infinite + s1) <==> Infinite)
  property("`+` is associative")   = associative[SI](_ + _)
  property("`max` is associative") = associative[SI](_ max _)
  property("`min` is associative") = associative[SI](_ min _)
  property("`+` is commutative")   = commutative[SI](_ + _)
  property("`max` is commutative") = commutative[SI](_ max _)
  property("`min` is commutative") = commutative[SI](_ min _)
}
