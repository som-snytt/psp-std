package psp
package std
package infix

import api._
import java.{ lang => jl }

final class OrderOps[A](val lhs: A) extends AnyVal {
  import Cmp._
  def compare(rhs: A)(implicit ord: Order[A]): Cmp = ord.compare(lhs, rhs)

  def < (rhs: A)(implicit ord: Order[A]): Boolean = compare(rhs) == LT
  def <=(rhs: A)(implicit ord: Order[A]): Boolean = compare(rhs) != GT
  def > (rhs: A)(implicit ord: Order[A]): Boolean = compare(rhs) == GT
  def >=(rhs: A)(implicit ord: Order[A]): Boolean = compare(rhs) != LT

  // Having implicit infix max and min interferes with having implicit
  // postfix max and min on collections.
  def min2(rhs: A)(implicit ord: Order[A]): A = if (this < rhs) lhs else rhs
  def max2(rhs: A)(implicit ord: Order[A]): A = if (this > rhs) lhs else rhs
}
final class PartialOrderOps[A](val lhs: A) extends AnyVal {
  def partialCompare(rhs: A)(implicit ord: PartialOrder[A]): PCmp    = ord.partialCompare(lhs, rhs)
  def tryCompare(rhs: A)(implicit ord: PartialOrder[A]): Option[Cmp] = partialCompare(rhs) match {
    case PCmp.LT => Some(Cmp.LT)
    case PCmp.GT => Some(Cmp.GT)
    case PCmp.EQ => Some(Cmp.EQ)
    case _       => None
  }

  def p_==(rhs: A)(implicit ord: PartialOrder[A]): Boolean = partialCompare(rhs) == PCmp.EQ
  def p_< (rhs: A)(implicit ord: PartialOrder[A]): Boolean = partialCompare(rhs) == PCmp.LT
  def p_<=(rhs: A)(implicit ord: PartialOrder[A]): Boolean = tryCompare(rhs) exists (_ != Cmp.GT)
  def p_> (rhs: A)(implicit ord: PartialOrder[A]): Boolean = partialCompare(rhs) == PCmp.GT
  def p_>=(rhs: A)(implicit ord: PartialOrder[A]): Boolean = tryCompare(rhs) exists (_ != Cmp.LT)

  def pmin(rhs: A)(implicit ord: PartialOrder[A]): Option[A] = tryCompare(rhs) map { case Cmp.GT => rhs ; case _ => lhs }
  def pmax(rhs: A)(implicit ord: PartialOrder[A]): Option[A] = tryCompare(rhs) map { case Cmp.LT => rhs ; case _ => lhs }
}
final class AlgebraOps[A](val lhs: A) extends AnyVal {
  def ^(rhs: A)(implicit z: BooleanAlgebra[A]): A       = (lhs || rhs) && !(lhs && rhs) // xor
  def implies(rhs: A)(implicit z: BooleanAlgebra[A]): A = !lhs || rhs
  def && (rhs: A)(implicit z: BooleanAlgebra[A]): A     = if (isOne) rhs else if (rhs.isOne) lhs else if (lhs.isZero || rhs.isZero) z.zero else z.and(lhs, rhs)
  def || (rhs: A)(implicit z: BooleanAlgebra[A]): A     = if (isZero) rhs else if (rhs.isZero) lhs else if (lhs.isOne || rhs.isOne) z.one else z.or(lhs, rhs)
  def unary_!(implicit z: BooleanAlgebra[A]): A         = if (isZero) z.one else if (isOne) z.zero else z.not(lhs)
  def isZero(implicit z: BooleanAlgebra[A]): Boolean    = z.zero id_== lhs
  def isOne(implicit z: BooleanAlgebra[A]): Boolean     = z.one id_== lhs
}
final class EqOps[A](val lhs: A) extends AnyVal {
  def ===(rhs: A)(implicit eq: Eq[A]): Boolean = eq.equiv(lhs, rhs)
  def !==(rhs: A)(implicit eq: Eq[A]): Boolean = !eq.equiv(lhs, rhs)
}
final class HashOps[A](val lhs: A) extends AnyVal {
  def hash(implicit z: Hash[A]): Int = z hash lhs
}
final class BuildsOps[A, That](z: Builds[A, That]) {
  def map[Next](f: That => Next): Builds[A, Next] = Builds(xs => f(z build xs))
}
