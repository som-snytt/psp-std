package psp
package std

import api._

object Algebras {
  final case class Mapped[R, S](algebra: BooleanAlgebra[R], f: S => R, g: R => S) extends BooleanAlgebra[S] {
    def and(x: S, y: S): S = g(algebra.and(f(x), f(y)))
    def or(x: S, y: S): S  = g(algebra.or(f(x), f(y)))
    def not(x: S): S       = g(algebra.not(f(x)))
    def zero: S            = g(algebra.zero)
    def one: S             = g(algebra.one)
  }
  object Identity extends BooleanAlgebra[Boolean] {
    def and(x: Boolean, y: Boolean): Boolean = x && y
    def or(x: Boolean, y: Boolean): Boolean  = x || y
    def not(x: Boolean): Boolean             = !x
    def zero: Boolean                        = false
    def one: Boolean                         = true
  }
  final class Predicate[A] extends BooleanAlgebra[psp.std.Predicate[A]] {
    private type R = psp.std.Predicate[A]

    def and(x: R, y: R): R = p => x(p) && y(p)
    def or(x: R, y: R): R  = p => x(p) || y(p)
    def not(x: R): R       = p => !x(p)
    def zero: R            = ConstantFalse
    def one: R             = ConstantTrue
  }
  final class ScalaSet[A] extends BooleanAlgebra[sciSet[A]] {
    import ScalaSet._
    def and(x: sciSet[A], y: sciSet[A]): sciSet[A] = (x, y) match {
      case (Complement(xs), Complement(ys)) => !(xs || ys)
      case (Complement(xs), ys)             => Difference(ys, xs)
      case (xs, Complement(ys))             => Difference(xs, ys)
      case _                                => x intersect y
    }
    def or(x: sciSet[A], y: sciSet[A]): sciSet[A] = (x, y) match {
      case (Complement(xs), Complement(ys)) => !(xs && ys)
      case (Complement(xs), ys)             => not(Difference(xs, ys))
      case (xs, Complement(ys))             => not(Difference(ys, xs))
      case _                                => x union y
    }
    def not(x: sciSet[A]): sciSet[A] = x match {
      case Zero           => one
      case One            => zero
      case Complement(xs) => xs            // unwrap
      case _              => Complement(x) // wrap
    }
    def zero: sciSet[A] = Zero.castTo[sciSet[A]]
    def one: sciSet[A]  = One.castTo[sciSet[A]]
  }
}
