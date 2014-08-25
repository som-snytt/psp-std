package psp

import psp.std._
import org.scalacheck._, Gen._, Prop._
import SizeInfo._

package object tests {
  type BinOp[A]           = (A, A) => A
  type Forall1[-A]        = A => Boolean
  type Forall2[-A]        = (A, A) => Boolean
  type Forall3[-A]        = (A, A, A) => Boolean
  type Prop               = org.scalacheck.Prop
  val Pretty              = org.scalacheck.util.Pretty
  type Result             = org.scalacheck.Test.Result
  type Failed             = org.scalacheck.Test.Failed
  type Buildable[A, C[X]] = org.scalacheck.util.Buildable[A, C]

  implicit def buildsToBuildable[CC[X], A](implicit z: Builds[A, CC[A]]): Buildable[A, CC] =
    new Buildable[A, CC] { def builder = Vector.newBuilder[A] mapResult (r => z build Foreach.elems(r: _*)) }

  implicit class GenOps[A](gen: Gen[A]) {
    def collect[B](pf: A ?=> B): Gen[B] = gen suchThat pf.isDefinedAt map pf.apply
  }
  implicit class PropOps(p: Prop) {
    def unary_! : Prop = p map (r => !r)
  }
  implicit class PropResultOps(r: Prop.Result) {
    def unary_! : Prop.Result = r.copy(status = !r.status)
  }
  implicit class StatusOps(s: Prop.Status) {
    def unary_! : Prop.Status = s match {
      case Proof => False
      case True  => False
      case False => True
      case x     => x
    }
  }

  def genSize: Gen[Size]         = chooseNum(1, Int.MaxValue / 2) map (n => Size(n))
  def genPrecise: Gen[Precise]   = genSize map (s => Precise(s))
  def genBounded: Gen[Bounded]   = genSize flatMap (lo => genAtomic map (hi => bounded(lo, hi))) collect { case b: Bounded => b }
  def genAtomic: Gen[Atomic]     = frequency(10 -> genPrecise, 1 -> Empty, 1 -> Infinite)
  def genSizeInfo: Gen[SizeInfo] = oneOf(genAtomic, genBounded)

  def genLong: Gen[Long]  = Gen.choose(MinLong, MaxLong)
  def genInt: Gen[Int]    = Gen.choose(MinInt, MaxInt)
  def genPosInt: Gen[Int] = Gen.choose(0, MaxInt)
  def genUInt: Gen[UInt]  = genInt map UInt
}
