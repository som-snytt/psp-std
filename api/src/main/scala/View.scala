package psp
package std
package api

import ApiAliases._

trait AnyView[+A] extends Any with Each[A] {
  type MapTo[+X] <: AnyView[X]
}

trait SetView[A] extends Any with AnyView[A] with ExSet[A] {
}

trait View[+A] extends Any with AnyView[A] {
  type MapTo[+X] <: View[X]

  def ++[A1 >: A](that: View[A1]): View[A1]
  def collect[B](pf: A ?=> B): MapTo[B]
  def drop(n: Precise): MapTo[A]
  def dropRight(n: Precise): MapTo[A]
  def dropWhile(p: Predicate[A]): MapTo[A]
  def flatMap[B](f: A => Each[B]): MapTo[B]
  def map[B](f: A => B): MapTo[B]
  def take(n: Precise): MapTo[A]
  def takeRight(n: Precise): MapTo[A]
  def takeWhile(p: Predicate[A]): MapTo[A]
  def viewOps: Direct[String]
  def withFilter(p: Predicate[A]): MapTo[A]
}

trait InvariantView[A] extends Any with View[A] {
  def join(that: InvariantView[A]): InvariantView[A]
  def partition(p: Predicate[A]): Split[A]
  def span(p: Predicate[A]): Split[A]
  def splitAt(index: Index): Split[A]
}

trait InMapView[-K, +V] extends Any with AnyView[V] with InMap[K, V] {
  type CoMapTo[-X] <: InMapView[X, V]
  type MapTo[+X] <: InMapView[K, X]

  def comap[K1](f: K1 => K): CoMapTo[K1]
  def filter(p: Predicate[V]): MapTo[V]
  def map[V1](f: V => V1): MapTo[V1]
}

final case class Split[A](left: View[A], right: View[A]) {
  def mapLeft(f: View[A] => View[A]): Split[A]  = Split(f(left), right)
  def mapRight(f: View[A] => View[A]): Split[A] = Split(left, f(right))
  def rejoin: View[A]                           = left ++ right
}
