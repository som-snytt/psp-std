package psp
package std
package api

import ApiAliases._

trait View[+A] extends Any with Foreach[A] {
  type MapTo[+X] <: View[X]

  def ++[A1 >: A](that: View[A1]): MapTo[A1]
  def calls: Int
  def collect[B](pf: A ?=> B): MapTo[B]
  def count(p: Predicate[A]): Precise
  def description: String
  def drop(n: Int): MapTo[A]
  def dropRight(n: Int): MapTo[A]
  def dropWhile(p: Predicate[A]): MapTo[A]
  def filter(p: Predicate[A]): MapTo[A]
  def filterNot(p: Predicate[A]): MapTo[A]
  def flatMap[B](f: A => Foreach[B]): MapTo[B]
  def map[B](f: A => B): MapTo[B]
  def sized(size: api.Size): MapTo[A]
  def slice(range: IndexRange): MapTo[A]
  def take(n: Int): MapTo[A]
  def takeRight(n: Int): MapTo[A]
  def takeWhile(p: Predicate[A]): MapTo[A]
  def viewChain: Foreach[View[_]]
  def viewRange: IndexRange
  def withFilter(p: Predicate[A]): MapTo[A]
}

object View {
  trait Atomic[+A] extends Any with View[A]
  trait Composite[A, +B] extends Any with View[B] { def prev: View[A] }
}
