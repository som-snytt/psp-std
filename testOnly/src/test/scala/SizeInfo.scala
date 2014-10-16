package psp
package tests

import org.scalacheck._, Prop._, Gen._
import psp.std._, api._

trait PspArb1                 { implicit def arbSizeInfo: Arbitrary[SizeInfo]= Arbitrary(genSizeInfo) }
trait PspArb2 extends PspArb1 { implicit def arbAtomic: Arbitrary[Atomic]    = Arbitrary(genAtomic)   }
trait PspArb3 extends PspArb2 { implicit def arbSize: Arbitrary[PreciseSize] = Arbitrary(genPrecise)  }

class SizeInfoSpec extends ScalacheckBundle with PspArb3 {
  type SI = SizeInfo
  type BinOp[T] = (T, T) => T
  type Tried[T] = scala.Either[Throwable, T]

  private def tried[T](op: => T) = try scala.Right(op) catch { case t: Throwable => scala.Left(t) }

  // When testing e.g. associativity and the sum overflows, we
  // need to do more than compare values for equality.
  private def sameOutcome[T](p1: => T, p2: => T): Boolean = (tried(p1), tried(p2)) match {
    case (scala.Right(x1), scala.Right(x2)) => x1 == x2
    case (scala.Left(t1), scala.Left(t2))   => t1.getClass == t2.getClass
    case _                                  => false
  }

  def commutative[T: Arbitrary](op: BinOp[T]): Prop = forAll((p1: T, p2: T) => sameOutcome(op(p1, p2), op(p2, p1)))
  def associative[T: Arbitrary](op: BinOp[T]): Prop = forAll((p1: T, p2: T, p3: T) => sameOutcome(op(op(p1, p2), p3), op(p1, op(p2, p3))))
  def certain[T: Arbitrary, U: Arbitrary](f: (T, U) => Boolean): Prop = forAll((p1: T, p2: U) => f(p1, p2))

  def flip(r: Prop.Result): Prop.Result = r match {
    case Prop.Result(Prop.True, _, _, _)  => r.copy(status = Prop.False)
    case Prop.Result(Prop.False, _, _, _) => r.copy(status = Prop.True)
    case _                                => r
  }

  def bundle = "SizeInfo"
  // ...Aaaaand right on cue, a bunch of these tests broke until I added a type annotation.
  def props = Seq[NamedProp](
    "s1 <= (s1 max s2)"    -> certain[Atomic, Atomic]((s1, s2) => (s1: SI) p_<= (s1 max s2)),
    "s1 >= (s1 min s2)"    -> certain[Atomic, Atomic]((s1, s2) => (s1: SI) p_>= (s1 min s2)),
    "s1 <= (s1 + s2)"      -> certain[Atomic, Atomic]((s1, s2) => (s1: SI) p_<= (s1 + s2)),
    "s1 >= (s1 - s2)"      -> certain[Atomic, PreciseSize]((s1, s2) => (s1: SI) p_>= (s1 - s2)),
    "<inf> + n"            -> forAll((s1: SI) => ((Infinite + s1) partialCompare Infinite) == PCmp.EQ),
    "`+` is associative"   -> associative[SI](_ + _),
    "`max` is associative" -> associative[SI](_ max _),
    "`min` is associative" -> associative[SI](_ min _),
    "`+` is commutative"   -> commutative[SI](_ + _),
    "`max` is commutative" -> commutative[SI](_ max _),
    "`min` is commutative" -> commutative[SI](_ min _)
  )
}
