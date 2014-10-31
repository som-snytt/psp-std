package psp
package tests

import compat.ScalaNative
import psp.std._, api._, StdShow._
import lowlevel.ExclusiveIntRange
import scala.{ collection => sc }
import sc.{ mutable => scm, immutable => sci }

/** Methods for comparing against scala views.
 */
trait TestedViewMethods extends ForceShowDirect {
  type This <: TestedViewMethods

  def name: String

  def collect(pf: Int ?=> Int): This
  def drop(n: Precise): This
  def dropRight(n: Precise): This
  def dropWhile(p: Predicate[Int]): This
  def filter(f: Predicate[Int]): This
  def filterNot(f: Predicate[Int]): This
  def flatMap(f: Int => Each[Int]): This
  def foreach(f: Int => Unit): Unit
  def map(f: Int => Int): This
  // def reverse: This
  def slice(range: IndexRange): This
  def take(n: Precise): This
  def takeRight(n: Precise): This
  def takeWhile(p: Predicate[Int]): This
  def withFilter(p: Predicate[Int]): This
}

final case class TestScalaView(name: String, xs: scIterable[Int]) extends TestedViewMethods {
  type This = TestScalaView
  private implicit def liftResult(xs: scIterable[Int]): This = copy(xs = xs)

  def collect(pf: Int ?=> Int)      = xs collect pf
  def drop(n: Precise)              = xs drop n.getInt
  def dropRight(n: Precise)         = xs dropRight n.getInt
  def dropWhile(p: Predicate[Int])  = xs dropWhile p
  def filter(p: Predicate[Int])     = xs filter p
  def filterNot(p: Predicate[Int])  = xs filterNot p
  def flatMap(f: Int => Each[Int])  = xs flatMap (x => f(x).toScalaTraversable)
  def foreach(f: Int => Unit)       = xs foreach f
  def map(f: Int => Int)            = xs map f
  // def reverse                       = xs.reverse
  def slice(range: IndexRange)      = xs slice (range.startInt, range.endInt)
  def take(n: Precise)              = xs take n.getInt
  def takeRight(n: Precise)         = xs takeRight n.getInt
  def takeWhile(p: Predicate[Int])  = xs takeWhile p
  def withFilter(p: Predicate[Int]) = xs filter p
  def to_s: String                  = xs mkString ("[ ", ", ", " ]")
}

final case class TestPolicyView(name: String, xs: View[Int]) extends TestedViewMethods {
  type This = TestPolicyView
  private implicit def liftResult(xs: View[Int]): This = copy(xs = xs)

  def collect(pf: Int ?=> Int)      = xs collect pf
  def drop(n: Precise)              = xs drop n
  def dropRight(n: Precise)         = xs dropRight n
  def dropWhile(p: Predicate[Int])  = xs dropWhile p
  def filter(p: Predicate[Int])     = xs filter p
  def filterNot(p: Predicate[Int])  = xs filterNot p
  def flatMap(f: Int => Each[Int])  = xs flatMap f
  def foreach(f: Int => Unit)       = xs foreach f
  def map(f: Int => Int)            = xs map f
  // def reverse                       = xs.reverse
  def slice(range: IndexRange)      = xs slice range
  def take(n: Precise)              = xs take n
  def takeRight(n: Precise)         = xs takeRight n
  def takeWhile(p: Predicate[Int])  = xs takeWhile p
  def withFilter(p: Predicate[Int]) = xs withFilter p
  def to_s: String                  = "[ " + (xs mk_s ", ") + " ]"
}
