package psp
package std

/** A boolean algebra abstracts across the standard boolean operations.
 *  The given laws must hold.
 */
trait BooleanAlgebra[R] {
  // a ∨ (b ∨ c) = (a ∨ b) ∨ c, a ∧ (b ∧ c) = (a ∧ b) ∧ c                associativity
  // a ∨ b = b ∨ a, a ∧ b = b ∧ a                                        commutativity
  // a ∨ (a ∧ b) = a, a ∧ (a ∨ b) = a                                    absorption
  // a ∨ 0 = a, a ∧ 1 = a                                                identity
  // a ∨ (b ∧ c) = (a ∨ b) ∧ (a ∨ c), a ∧ (b ∨ c) = (a ∧ b) ∨ (a ∧ c)    distributivity
  // a ∨ ¬a = 1, a ∧ ¬a = 0                                              complements
  def and(x: R, y: R): R
  def or(x: R, y: R): R
  def not(x: R): R
  def zero: R
  def one: R
}

object BooleanAlgebra {
  final case object ConstantTrue extends Predicate[Any] { def apply(x: Any): Boolean = true }
  final case object ConstantFalse extends Predicate[Any] { def apply(x: Any): Boolean = false }

  final case class MappedAlgebra[R, S](algebra: BooleanAlgebra[R], f: S => R, g: R => S) extends BooleanAlgebra[S] {
    def and(x: S, y: S): S = g(algebra.and(f(x), f(y)))
    def or(x: S, y: S): S  = g(algebra.or(f(x), f(y)))
    def not(x: S): S       = g(algebra.not(f(x)))
    def zero: S            = g(algebra.zero)
    def one: S             = g(algebra.one)
  }

  implicit object IdentityAlgebra extends BooleanAlgebra[Boolean] {
    def and(x: Boolean, y: Boolean): Boolean = x && y
    def or(x: Boolean, y: Boolean): Boolean  = x || y
    def not(x: Boolean): Boolean             = !x
    def zero: Boolean                        = false
    def one: Boolean                         = true
  }
  class PredicateAlgebra[A] extends BooleanAlgebra[Predicate[A]] {
    private type R = Predicate[A]

    def and(x: R, y: R): R = p => x(p) && y(p)
    def or(x: R, y: R): R  = p => x(p) || y(p)
    def not(x: R): R       = p => !x(p)
    def zero: R            = ConstantFalse
    def one: R             = ConstantTrue
  }
  implicit def predicateAlgebra[A] : PredicateAlgebra[A] = new PredicateAlgebra[A]
  implicit def scalaSetAlgebra[A] : ScalaSet.Algebra[A]  = new ScalaSet.Algebra[A]
}

object ScalaSet {
  type ScalaSet[A] = scala.collection.immutable.Set[A]
  def isSubSet[A: Eq](xs: Set[A], ys: Set[A]): Boolean = xs forall (x => ys exists (y => x === y))

  implicit def scalaSetEq[CC[X] <: Set[X], A : Eq] : Eq[CC[A]] = Eq[CC[A]] {
    case (Complement(xs), Complement(ys)) => isSubSet(xs, ys) && isSubSet(ys, xs)
    case (Complement(xs), y)              => false
    case (x, Complement(ys))              => false
    case (xs, ys)                         => isSubSet(xs, ys) && isSubSet(ys, xs)
  }

  private val Zero: ScalaSet[Any] = new EmptySet[Any]
  private val One: ScalaSet[Any]  = new Complement[Any](Zero)
  private class EmptySet[A] extends ScalaSet[A] {
    def iterator          = Iterator[A]()
    def -(elem: A)        = this
    def +(elem: A)        = Set(elem)
    def contains(elem: A) = false
    override def toString = "Ø"
  }

  class Algebra[A] extends BooleanAlgebra[ScalaSet[A]] {
    def and(x: ScalaSet[A], y: ScalaSet[A]): ScalaSet[A] = (x, y) match {
      case (Complement(xs), Complement(ys)) => !(xs || ys)
      case (Complement(xs), ys)             => Difference(ys, xs)
      case (xs, Complement(ys))             => Difference(xs, ys)
      case _                                => x intersect y
    }
    def or(x: ScalaSet[A], y: ScalaSet[A]): ScalaSet[A] = (x, y) match {
      case (Complement(xs), Complement(ys)) => !(xs && ys)
      case (Complement(xs), ys)             => not(Difference(xs, ys))
      case (xs, Complement(ys))             => not(Difference(ys, xs))
      case _                                => x union y
    }
    def not(x: ScalaSet[A]): ScalaSet[A] = x match {
      case Zero           => one
      case One            => zero
      case Complement(xs) => xs            // unwrap
      case _              => Complement(x) // wrap
    }
    def zero: ScalaSet[A] = Zero.castTo[ScalaSet[A]]
    def one: ScalaSet[A]  = One.castTo[ScalaSet[A]]
  }

  final case class Difference[A](lhs: Set[A], rhs: Set[A]) extends ScalaSet[A] {
    def iterator           = lhs.iterator filterNot rhs
    def -(elem: A): Set[A] = if (lhs(elem)) Difference(lhs, rhs + elem) else this
    def +(elem: A): Set[A] = if (lhs(elem) && !rhs(elem)) this else Difference(lhs + elem, rhs - elem)
    def contains(elem: A)  = lhs(elem) && !rhs(elem)
    override def toString = lhs match {
      case Complement(_) => s"($lhs) / $rhs"
      case _             => (lhs filterNot rhs).toString
    }
  }
  final case class Complement[A](xs: Set[A]) extends ScalaSet[A] {
    // The repl calls size to see if it needs to truncate the string, and then
    // the size implementation calls iterator.
    override def size      = Int.MaxValue
    def iterator           = sys.error("Cannot iterate over set complement")
    def -(elem: A): Set[A] = !(xs + elem)
    def +(elem: A): Set[A] = !(xs - elem)
    def contains(elem: A)  = !(xs contains elem)

    // It's impossible to meet the hashcode contract.
    override def hashCode = ~(xs.##)
    override def equals(x: Any): Boolean = x match {
      case Complement(ys) => xs == ys
      case _              => false
    }
    override def toString = xs match {
      case Zero => "U"
      case _    => s"U ∖ $xs"
    }
  }
}
