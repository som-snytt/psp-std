package psp
package std

import api._

final class Label(val label: String) extends AnyVal {
  def matches(r: Regex)   = r isMatch label
  def contains(s: String) = label contains s
  def containsOp          = contains("&&") || contains("||") || (label startsWith "!")
  def isSafe              = matches("""^[(](.*?)[)]$""".r) || !containsOp
  def isBool              = isZero || isOne
  def isZero              = label eq Label.Zero.label
  def isOne               = label eq Label.One.label

  override def toString = label
}
object Label {
  val Zero = new Label(new String(""))
  val One  = new Label(new String(""))
  def apply(s: String) = new Label(s)
}

/** TODO - how to abstract the entire notion of a Complement class and the
 *  always-the-same logic which accompanies it, without virtual classes?
 */
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
  object LabelAlgebra extends BooleanAlgebra[Label] {
    private def maybeParens(lhs: Label, op: String, rhs: Label): String =
      if (lhs.isSafe && rhs.isSafe) s"$lhs $op $rhs" else s"($lhs $op $rhs)"

    def and(x: Label, y: Label): Label = if (x.isZero || y.isZero) zero else if (x.isOne) y else if (y.isOne) x else Label(maybeParens(x, "&&", y))
    def or(x: Label, y: Label): Label  = if (x.isBool && !y.isBool) y else if (y.isBool && !x.isBool) x else Label(maybeParens(x, "||", y))
    def not(x: Label): Label           = Label( if (x.isSafe) s"!$x" else "!($x)" )
    def zero                           = Label.Zero
    def one                            = Label.One
  }

  final case class PredicateComplement[A](f: psp.std.Predicate[A]) extends psp.std.Predicate[A] {
    def apply(x: A): Boolean = !f(x)
    override def toString = "!" + f
  }
  final class Predicate[A] extends BooleanAlgebra[psp.std.Predicate[A]] {
    private type R = psp.std.Predicate[A]

    /** TODO - one of of the benefits of having constant true and false is an
     *  opportunity to optimize expressions away entirely with no evaluation,
     *  if e.g. y is ConstantTrue in x(p) || y(p). Obviously this won't mix well
     *  with side effects. How enthusiastic can we be about punishing side effects
     *  before we kill the patient?
     */
    def and(x: R, y: R): R = p => x(p) && y(p)
    def or(x: R, y: R): R  = p => x(p) || y(p)
    def zero: R            = ConstantFalse
    def one: R             = ConstantTrue
    def not(f: R): R       = f match {
      case ConstantFalse          => ConstantTrue
      case ConstantTrue           => ConstantFalse
      case PredicateComplement(f) => f
      case _                      => PredicateComplement(f)
    }
  }
  // TODO - recover this logic for PolicySet.
  //
  // final class ScalaSet[A] extends BooleanAlgebra[sciSet[A]] {
  //   import ScalaSet._
  //   def and(x: sciSet[A], y: sciSet[A]): sciSet[A] = (x, y) match {
  //     case (Complement(xs), Complement(ys)) => !(xs || ys)
  //     case (Complement(xs), ys)             => Difference(ys, xs)
  //     case (xs, Complement(ys))             => Difference(xs, ys)
  //     case _                                => x intersect y
  //   }
  //   def or(x: sciSet[A], y: sciSet[A]): sciSet[A] = (x, y) match {
  //     case (Complement(xs), Complement(ys)) => !(xs && ys)
  //     case (Complement(xs), ys)             => not(Difference(xs, ys))
  //     case (xs, Complement(ys))             => not(Difference(ys, xs))
  //     case _                                => x union y
  //   }
  //   def not(x: sciSet[A]): sciSet[A] = x match {
  //     case Zero           => one
  //     case One            => zero
  //     case Complement(xs) => xs            // unwrap
  //     case _              => Complement(x) // wrap
  //   }
  //   def zero: sciSet[A] = Zero.castTo[sciSet[A]]
  //   def one: sciSet[A]  = One.castTo[sciSet[A]]
  // }
}
