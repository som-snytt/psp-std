package psp

import org.scalacheck._, Prop._
import psp.std._, api._
import StdShow._

package object tests {
  lazy val isTestDebug = sys.props contains "psp.test.debug"

  type Arb[A]             = Arbitrary[A]
  val Arb                 = Arbitrary
  type Forall1[-A]        = Predicate[A]
  type Forall2[-A]        = Relation[A]
  type Forall3[-A]        = (A, A, A) => Boolean
  type Prop               = org.scalacheck.Prop
  val Prop                = org.scalacheck.Prop
  val Pretty              = org.scalacheck.util.Pretty
  type Result             = org.scalacheck.Test.Result
  type Failed             = org.scalacheck.Test.Failed
  type Buildable[A, C[X]] = org.scalacheck.util.Buildable[A, C]

  def arb[A](implicit z: Arb[A]): Arb[A] = z

  implicit class ArbitraryOps[A](x: Arb[A]) {
    def map[B](f: A => B): Arb[B]       = Arb(x.arbitrary map f)
    def filter(p: Predicate[A]): Arb[A] = Arb(x.arbitrary filter p)
  }
  implicit def arbitraryInSet[A : Arb : HashEq] : Arb[InSet[A]] = arb[sciSet[A]] map (_.m.toPolicySet)
  implicit def arbitraryPint: Arb[Pint]                         = Arb(Gen.choose(MinInt, MaxInt) map (x => Pint(x)))
  implicit class LiftConverter[A](gen: Gen[A]) {
    def to[B](implicit f: A => B): Gen[B] = gen map f
  }


  def preNewline(s: String) = if (s contains "\n") "\n" + s.mapLines("| " ~ _) else s
  def showsAs[A: Show](expected: String, x: A): NamedProp         = preNewline(expected) -> (expected =? show"$x")
  def seqShows[A: Show](expected: String, xs: Each[A]): NamedProp = preNewline(expected) -> (expected =? (xs mk_s ", "))

  def expectType(expected: jClass, found: jClass): NamedProp        = fshow"$expected%15s  >:>  $found%s" -> Prop(expected isAssignableFrom found)
  def expectTypes(expected: jClass, found: Each[jClass]): NamedProp = fshow"$expected%15s  >:>  $found%s" -> found.map(c => Prop(expected isAssignableFrom c))

  def expectType[A: CTag](result: A): NamedProp                     = expectType(classOf[A], result.getClass)
  def expectTypes[A: CTag](results: A*): NamedProp                  = expectTypes(classOf[A], fromScala(results) map (_.getClass))

  implicit def buildsToBuildable[A, CC[X]](implicit z: Builds[A, CC[A]]): Buildable[A, CC] =
    new Buildable[A, CC] { def builder: scmBuilder[A, CC[A]] = z.scalaBuilder }

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
}

package tests {
  final case class Pint(x: Int) { override def toString = s"$x" }
}
