package psp
package stdtests

import psp.std._
import org.scalacheck._
import org.scalacheck.Prop.forAll
import org.scalacheck.util.{ Pretty, ConsoleReporter }
import Test.{ Result, Failed, TestCallback }

object AlgebraPoliceman {
  def pp(r: Result)                           = Pretty.pretty(r, Pretty.Params(0))
  def top(label: String, prop: Prop): Boolean = Test.check(prop)(identity) match {
    case r @ Result(Failed(args, labels), succeeded, discarded, _, _) => andFalse(println(s"Falsified after $succeeded passed tests.\n" + pp(r)))
    case x                                                            => try x.passed finally println(s"+ OK, passed ${x.succeeded} tests.     $label")
  }

  def check[A : BooleanAlgebra : Arbitrary : Eq](name: String) = new AlgebraPoliceman[A](name) checkAll top
}

class AlgebraPoliceman[A : BooleanAlgebra : Arbitrary : Eq](name: String) {
  type BinOp[A]    = (A, A) => A
  type Forall1[-A] = A => Boolean
  type Forall2[-A] = (A, A) => Boolean
  type Forall3[-A] = (A, A, A) => Boolean

  val algebra = implicitly[BooleanAlgebra[A]]
  import algebra._

  def associative(f: BinOp[A]): Forall3[A]               = (a, b, c) => f(a, f(b, c)) === f(f(a, b), c)
  def distributive(f: BinOp[A], g: BinOp[A]): Forall3[A] = (a, b, c) => f(a, g(b, c)) === g(f(a, b), f(a, c))
  def commutative(f: BinOp[A]): Forall2[A]               = (a, b)    => f(a, b) === f(b, a)
  def absorption(f: BinOp[A], g: BinOp[A]): Forall2[A]   = (a, b)    => f(a, g(a, b)) === a
  def identity(f: BinOp[A], id: A): Forall1[A]           = a         => f(a, id) === a
  def complement(f: BinOp[A], id: A): Forall1[A]         = a         => f(a, !a) === id

  val props = List[(String, Prop)](
    "a ∧ (b ∧ c) = (a ∧ b) ∧ c"       -> forAll(associative(and)),
    "a ∨ (b ∨ c) = (a ∨ b) ∨ c"       -> forAll(associative(or)),
    "a ∧ b = b ∧ a"                   -> forAll(commutative(and)),
    "a ∨ b = b ∨ a"                   -> forAll(commutative(or)),
    "a ∧ (b ∨ c) = (a ∧ b) ∨ (a ∧ c)" -> forAll(distributive(and, or)),
    "a ∨ (b ∧ c) = (a ∨ b) ∧ (a ∨ c)" -> forAll(distributive(or, and)),
    "a ∧ (a ∨ b) = a"                 -> forAll(absorption(and, or)),
    "a ∨ (a ∧ b) = a"                 -> forAll(absorption(or, and)),
    "a ∨ 0 = a"                       -> forAll(identity(or, zero)),
    "a ∧ 1 = a"                       -> forAll(identity(and, one)),
    "a ∨ ¬a = 1"                      -> forAll(complement(or, one)),
    "a ∧ ¬a = 0"                      -> forAll(complement(and, zero))
  )

  def checkAll(f: (String, Prop) => Boolean): Boolean = ( for ((label, prop) <- props) yield f("%-40s  %s".format(label, name), prop) ) forall (x => x)
}

class ScalacheckCallback extends Test.TestCallback {
  private def log(msg: String): Unit = ()
  override def onPropEval(name: String, threadIdx: Int, succeeded: Int, discarded: Int): Unit = {
    log(s"onPropEval($name, $threadIdx, $succeeded, $discarded)")
  }
  override def onTestResult(name: String, result: Test.Result): Unit = {
    log(s"onTestResult($name, $result)")
  }
  override def chain(testCallback: TestCallback): TestCallback = super.chain(testCallback)
}
