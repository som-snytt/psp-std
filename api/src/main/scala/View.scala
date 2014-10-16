package psp
package std
package api

import ApiAliases._

trait View[+A] extends Any with Foreach[A] with RearSliceable[View[A]] {
  type MapTo[+X] <: View[X]

  def ++[A1 >: A](that: View[A1]): MapTo[A1]
  def calls: Int
  def collect[B](pf: A ?=> B): MapTo[B]
  def description: String
  def drop(n: PreciseSize): MapTo[A]
  def dropRight(n: PreciseSize): MapTo[A]
  def dropWhile(p: Predicate[A]): MapTo[A]
  def filter(p: Predicate[A]): MapTo[A]
  def filterNot(p: Predicate[A]): MapTo[A]
  def flatMap[B](f: A => Foreach[B]): MapTo[B]
  def map[B](f: A => B): MapTo[B]
  def sized(size: PreciseSize): MapTo[A]
  def slice(range: IndexRange): MapTo[A]
  def take(n: PreciseSize): MapTo[A]
  def takeRight(n: PreciseSize): MapTo[A]
  def takeWhile(p: Predicate[A]): MapTo[A]
  def viewChain: Foreach[View[_]]
  def viewRange: IndexRange
  def withFilter(p: Predicate[A]): MapTo[A]
}

trait FrontSliceable[+A] extends Any {
  def drop(n: PreciseSize): A
  def take(n: PreciseSize): A
  def slice(range: IndexRange): A

  def drop(n: Int): A = drop(PreciseSize(n))
  def take(n: Int): A = take(PreciseSize(n))
}

trait RearSliceable[+A] extends Any with FrontSliceable[A] {
  def dropRight(n: PreciseSize): A
  def takeRight(n: PreciseSize): A
  def dropRight(n: Int): A = dropRight(PreciseSize(n))
  def takeRight(n: Int): A = takeRight(PreciseSize(n))
}

object View {
  trait Atomic[+A] extends Any with View[A]
  trait Composite[A, +B] extends Any with View[B] { def prev: View[A] }
}
