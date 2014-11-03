package psp
package tests

import psp.std._, api._, StdShow._
import lowlevel.ExclusiveIntRange
import scala.{ collection => sc }
import sc.{ mutable => scm, immutable => sci }

object ViewClass {
  type Op    = Unary[ViewClass]
  type Trans = Unary[Op]
}

/** Methods for comparing against scala views.
 */
trait ViewClass extends ForceShowDirect {
  type This <: ViewClass

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

final case class ScalaViewClass(name: String, xs: scIterable[Int]) extends ViewClass {
  type This = ScalaViewClass
  private implicit def liftResult(xs: scIterable[Int]): This = copy(xs = xs)

  def collect(pf: Int ?=> Int)      = xs collect pf
  def drop(n: Precise)              = xs drop n.safeInt
  def dropRight(n: Precise)         = xs dropRight n.safeInt
  def dropWhile(p: Predicate[Int])  = xs dropWhile p
  def filter(p: Predicate[Int])     = xs filter p
  def filterNot(p: Predicate[Int])  = xs filterNot p
  def flatMap(f: Int => Each[Int])  = xs flatMap (x => f(x).toScalaTraversable)
  def foreach(f: Int => Unit)       = xs foreach f
  def map(f: Int => Int)            = xs map f
  // def reverse                       = xs.reverse
  def slice(range: IndexRange)      = xs slice (range.startInt, range.endInt)
  def take(n: Precise)              = xs take n.safeInt
  def takeRight(n: Precise)         = xs takeRight n.safeInt
  def takeWhile(p: Predicate[Int])  = xs takeWhile p
  def withFilter(p: Predicate[Int]) = xs filter p
  def to_s: String                  = xs mkString ("[ ", ", ", " ]")
}

final case class PolicyViewClass(name: String, xs: View[Int]) extends ViewClass {
  type This = PolicyViewClass
  private implicit def liftResult(xs: View[Int]): This = copy(xs = xs)

  def collect(pf: Int ?=> Int)      = xs collect pf
  def drop(n: Precise)              = xs drop n
  def dropRight(n: Precise)         = xs dropRight n
  def dropWhile(p: Predicate[Int])  = xs dropWhile p
  def filter(p: Predicate[Int])     = this withFilter p
  def filterNot(p: Predicate[Int])  = this withFilter !p
  def flatMap(f: Int => Each[Int])  = xs flatMap f
  def foreach(f: Int => Unit)       = xs foreach f
  def map(f: Int => Int)            = xs map f
  // def reverse                       = xs.reverse
  def slice(range: IndexRange)      = xs drop range.toDrop take range.toTake
  def take(n: Precise)              = xs take n
  def takeRight(n: Precise)         = xs takeRight n
  def takeWhile(p: Predicate[Int])  = xs takeWhile p
  def withFilter(p: Predicate[Int]) = xs withFilter p
  def to_s: String                  = "[ " + (xs mk_s ", ") + " ]"
}
