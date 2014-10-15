package psp
package tests

import org.scalacheck._
import psp.std._, api._

abstract class Laws[A : Eq] {
  def associative(f: BinOp[A]): Forall3[A]               = (a, b, c) => f(a, f(b, c)) === f(f(a, b), c)
  def distributive(f: BinOp[A], g: BinOp[A]): Forall3[A] = (a, b, c) => f(a, g(b, c)) === g(f(a, b), f(a, c))
  def commutative(f: BinOp[A]): Forall2[A]               = (a, b)    => f(a, b) === f(b, a)
  def absorption(f: BinOp[A], g: BinOp[A]): Forall2[A]   = (a, b)    => f(a, g(a, b)) === a
  def identity(f: BinOp[A], id: A): Forall1[A]           = a         => f(a, id) === a
  def idempotence(f: BinOp[A]): Forall1[A]               = a         => f(a, a) === a
}
abstract class AlgebraLaws[A : Eq : BooleanAlgebra] extends Laws[A] {
  def complement(f: BinOp[A], id: A): Forall1[A] = a => f(a, !a) === id
}

class ScalacheckCallback extends Test.TestCallback {
  private def log(msg: String): Unit = ()
  override def onPropEval(name: String, threadIdx: Int, succeeded: Int, discarded: Int): Unit = {
    log(s"onPropEval($name, $threadIdx, $succeeded, $discarded)")
  }
  override def onTestResult(name: String, result: Test.Result): Unit = {
    log(s"onTestResult($name, $result)")
  }
  override def chain(testCallback: Test.TestCallback): Test.TestCallback = super.chain(testCallback)
}

/***
trait RelationLaws[A] {
  implicit def arbA: Arbitrary[A]

  def reflexive(r: Relation[A])                        = forAll((a: A) => r(a, a))
  def transitive(r: Relation[A])                       = forAll((a: A, b: A, c: A) => r(a, b) && r(b, c) ==> r(a, c))
  def symmetric(r: Relation[A])                        = forAll((a: A, b: A) => r(a, b) ==> r(b, a))
  def antisymmetric(r: Relation[A])(implicit z: Eq[A]) = forAll((a: A, b: A) => r(a, b) && r(b, a) ==> z.equiv(a, b))
}
***/
