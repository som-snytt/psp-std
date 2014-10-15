package psp
package std

import api._

object PolicySet {
  def builder[A: HashEq] : Builds[A, PolicySet[A]] = Builds(xs => new PolicySet[A](xs))

  implicit def policySetEq[A : HashEq] : Eq[PolicySet[A]] = Eq[PolicySet[A]]((xs, ys) => (xs.size == ys.size) && (xs forall ys))

  def natural[A](xs: Foreach[A]): PolicySet[A]                                          = apply[A](xs)(HashEq.natural[A])
  def reference[A <: AnyRef](xs: Foreach[A]): PolicySet[A]                              = apply[A](xs)(HashEq.reference[A])
  def shown[A: Show](xs: Foreach[A]): PolicySet[A]                                      = apply[A](xs)(HashEq.shown[A])
  def direct[A](xs: Foreach[A])(equiv: (A, A) => Boolean, hash: A => Int): PolicySet[A] = apply[A](xs)(HashEq(equiv, hash))
  def apply[A: HashEq](xs: Foreach[A]): PolicySet[A]                                    = new PolicySet[A](xs)
}

/** XXX TODO - don't involve scala's set.
 */
final class PolicySet[A: HashEq](basis: Foreach[A]) extends sciSet[A] {
  private[this] val wrapSet: jSet[Wrap] = jSet(basis map wrap seq: _*)
  private def wrap(elem: A): Wrap = new Wrap(elem)
  private class Wrap(val unwrap: A) {
    final override def equals(that: Any): Boolean = that match {
      case x: Wrap => equiv(unwrap, x.unwrap)
      case _       => false
    }
    override def hashCode = hash(unwrap)
    override def toString = s"$unwrap (wrapped)"
  }
  def equiv(x: A, y: A): Boolean = x === y
  def hash(x: A): Int            = x.hash

  def toPolicySeq: pVector[A]             = Direct.builder[A](iterator foreach _)
  def addIfAbsent(elem: A): PolicySet[A]  = if (contains(elem)) this else this + elem
  def addOrReplace(elem: A): PolicySet[A] = if (contains(elem)) (this - elem) + elem else this + elem

  def by[B : HashEq](f: A => B): PolicySet[A]    = PolicySet[A](basis)(hashEqBy[A](f))
  def byNatural                                  = PolicySet.natural[A](basis)
  def byReference(implicit ev: A <:< AnyRef)     = PolicySet.reference(basis map ev)
  def byShown(implicit z: Show[A]): PolicySet[A] = PolicySet.shown[A](basis)

  // def grouped = wrapSet.toList groupBy (x => x) map { case (k, vs) => (k.unwrap, vs map (_.unwrap)) }

  override def size              = wrapSet.size
  def iterator: BiIterator[A]    = BiIterable(wrapSet).iterator map (_.unwrap)
  def contains(elem: A): Boolean = wrapSet contains wrap(elem)
  def -(elem: A)                 = this
  def +(elem: A)                 = this
}

object ScalaSet {
  final val Zero: sciSet[Any] = new EmptySet[Any]
  final val One: sciSet[Any]  = new Complement[Any](Zero)
  final class EmptySet[A] extends sciSet[A] {
    def iterator          = scIterator.empty
    def -(elem: A)        = this
    def +(elem: A)        = Set(elem)
    def contains(elem: A) = false
    override def toString = "Ø"
  }

  def isSubSet[A: Eq](xs: sciSet[A], ys: sciSet[A]): Boolean = xs forall (x => ys exists (y => x === y))

  implicit def scalaSetEq[CC[X] <: sciSet[X], A : Eq] : Eq[CC[A]] = Eq[CC[A]] {
    case (Complement(xs), Complement(ys)) => isSubSet(xs, ys) && isSubSet(ys, xs)
    case (Complement(xs), y)              => false
    case (x, Complement(ys))              => false
    case (xs, ys)                         => isSubSet(xs, ys) && isSubSet(ys, xs)
  }

  final case class Difference[A](lhs: sciSet[A], rhs: sciSet[A]) extends sciSet[A] {
    def iterator              = lhs.iterator filterNot rhs
    def -(elem: A): sciSet[A] = if (lhs(elem)) Difference(lhs, rhs + elem) else this
    def +(elem: A): sciSet[A] = if (lhs(elem) && !rhs(elem)) this else Difference(lhs + elem, rhs - elem)
    def contains(elem: A)     = lhs(elem) && !rhs(elem)
    override def toString = lhs match {
      case Complement(_) => s"($lhs) / $rhs"
      case _             => (lhs filterNot rhs).toString
    }
  }
  final case class Complement[A](xs: sciSet[A]) extends sciSet[A] {
    // The repl calls size to see if it needs to truncate the string, and then
    // the size implementation calls iterator.
    override def size         = MaxInt
    def iterator              = abortTrace("Cannot iterate over set complement")
    def -(elem: A): sciSet[A] = !(xs + elem)
    def +(elem: A): sciSet[A] = !(xs - elem)
    def contains(elem: A)     = !(xs contains elem)

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


// package psp
// package std

// import api._

// object EquivSet {
//   implicit def equivSetEq[A: HashEq]                          = Eq[EquivSet[A]]((xs, ys) => (xs.size == ys.size) && (xs forall ys))
//   implicit def newBuilder[A: HashEq] : Builds[A, EquivSet[A]] = Builds(xs => new EquivSet[A](xs))

//   def universal[A](xs: Foreach[A]): EquivSet[A]           = apply[A](xs)(HashEq.natural[A])
//   def reference[A <: AnyRef](xs: Foreach[A]): EquivSet[A] = apply[A](xs)(HashEq.reference[A])
//   def shown[A: Show](xs: Foreach[A]): EquivSet[A]         = apply[A](xs)(HashEq.shown[A])
//   def apply[A: HashEq](xs: Foreach[A]): EquivSet[A]       = new EquivSet[A](xs)
// }

// final class EquivSet[A : HashEq](basis: Foreach[A]) extends sciSet[A] {
//   private def hasheq: HashEq[A]   = ?
//   private[this] val wrapSet       = basis.m map wrap toScalaSet
//   private def wrap(elem: A): Wrap = new Wrap(elem)
//   private class Wrap(val unwrap: A) {
//     final override def equals(that: Any): Boolean = that match {
//       case x: Wrap => unwrap === x.unwrap
//       case _       => false
//     }
//     override def hashCode = hasheq hash unwrap
//     override def toString = s"$unwrap (wrapped)"
//   }

//   def byUniversal = EquivSet[A](basis)(HashEq.natural[A])
//   def byReference = EquivSet[A with AnyRef](basis.m map (_.castTo[A with AnyRef]))(HashEq.reference())
//   def byShown     = EquivSet[A](basis)(HashEq.shown[A]()(Show.native[A]))

//   def grouped = wrapSet.toList groupBy (x => x) map { case (k, vs) => (k.unwrap, vs map (_.unwrap)) }

//   override def size              = wrapSet.size
//   def iterator: scIterator[A]    = wrapSet.iterator map (_.unwrap)
//   def contains(elem: A): Boolean = wrapSet(wrap(elem))
//   def -(elem: A)                 = this
//   def +(elem: A)                 = this
// }

// object ScalaSet {
//   final val Zero: sciSet[Any] = new EmptySet[Any]
//   final val One: sciSet[Any]  = new Complement[Any](Zero)
//   final class EmptySet[A] extends sciSet[A] {
//     def iterator          = scIterator[A]()
//     def -(elem: A)        = this
//     def +(elem: A)        = Set(elem)
//     def contains(elem: A) = false
//     override def toString = "Ø"
//   }

//   def isSubSet[A: Eq](xs: sciSet[A], ys: sciSet[A]): Boolean = xs forall (x => ys exists (y => x === y))

//   implicit def scalaSetEq[CC[X] <: sciSet[X], A : Eq] : Eq[CC[A]] = Eq[CC[A]] {
//     case (Complement(xs), Complement(ys)) => isSubSet(xs, ys) && isSubSet(ys, xs)
//     case (Complement(xs), y)              => false
//     case (x, Complement(ys))              => false
//     case (xs, ys)                         => isSubSet(xs, ys) && isSubSet(ys, xs)
//   }

//   final case class Difference[A](lhs: sciSet[A], rhs: sciSet[A]) extends sciSet[A] {
//     def iterator              = lhs.iterator filterNot rhs
//     def -(elem: A): sciSet[A] = if (lhs(elem)) Difference(lhs, rhs + elem) else this
//     def +(elem: A): sciSet[A] = if (lhs(elem) && !rhs(elem)) this else Difference(lhs + elem, rhs - elem)
//     def contains(elem: A)     = lhs(elem) && !rhs(elem)
//     override def toString = lhs match {
//       case Complement(_) => s"($lhs) / $rhs"
//       case _             => (lhs filterNot rhs).toString
//     }
//   }
//   final case class Complement[A](xs: sciSet[A]) extends sciSet[A] {
//     // The repl calls size to see if it needs to truncate the string, and then
//     // the size implementation calls iterator.
//     override def size         = MaxInt
//     def iterator              = sys.error("Cannot iterate over set complement")
//     def -(elem: A): sciSet[A] = !(xs + elem)
//     def +(elem: A): sciSet[A] = !(xs - elem)
//     def contains(elem: A)     = !(xs contains elem)

//     // It's impossible to meet the hashcode contract.
//     override def hashCode = ~(xs.##)
//     override def equals(x: Any): Boolean = x match {
//       case Complement(ys) => xs == ys
//       case _              => false
//     }
//     override def toString = xs match {
//       case Zero => "U"
//       case _    => s"U ∖ $xs"
//     }
//   }
// }
