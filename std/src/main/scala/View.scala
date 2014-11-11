package psp
package std

import api._

object View {
  def apply(s: String): AnyView[Char]    = apply(Direct fromString s)
  def apply[A](xs: Array[A]): AnyView[A] = apply(Direct fromArray xs)
  def apply[A](xs: Each[A]): AnyView[A] = xs match {
    case xs: ExSet[A]   => exset(xs)
    case xs: Direct[A]  => direct(xs)
    // case xs: Indexed[A] => new IndexedView(xs)
    case xs: Linear[A]  => linear(xs)
    case _              => new LinearView(xs)
  }
  def fromScala[A](xs: sCollection[A]): AnyView[A] = xs match {
    case xs: sciIndexedSeq[_] => Direct fromScala xs m
    case xs: sciLinearSeq[_]  => Linear fromScala xs m
    case xs: sciSet[_]        => ExSet fromScala xs m
    case _                    => Each fromScala xs m
  }
  def fromJava[A](xs: jIterable[A]): AnyView[A] = xs match {
    case xs: jList[_] => Direct fromJava xs m
    case xs: jSet[_]  => ExSet fromJava xs m
    case xs           => Each fromJava xs m
  }

  def each[A, Repr](xs: Each[A]): LinearView[A, Repr]     = new LinearView(xs)
  def linear[A, Repr](xs: Linear[A]): LinearView[A, Repr] = new LinearView(xs)
  def direct[A, Repr](xs: Direct[A]): DirectView[A, Repr] = new DirectView(xs)
  def exset[A, Repr](xs: ExSet[A]): ExSetView[A, Repr]    = new ExSetView(xs)
}
