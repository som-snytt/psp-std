package psp
package std

import Nat._

/** We'd like to be able to work with short fixed-length lists without
 *  a bunch of unnecessary complexity and absent type/arity safety.
 *  We mix GeneralizedApply into the companion object and rely on implicit
 *  conversions from the object itself to the apply carriers.
 *
 *  The implicit prioritization is necessary to disambiguate a single argument.
 *  All the TupleN overloads are given first crack. So, in e.g. NatList:
 *
 *  scala> NatList((1, 2, 3))
 *  res0: psp.core.NatList[psp.std.Nat._3,Int] = 1 :: 2 :: 3 :: Nil
 *
 */
trait GeneralizedApply {
  val arity: ArityApplies
}
trait LowGeneralizedApply {
  implicit def arityApplies(x: GeneralizedApply): x.arity.type = x.arity
}
object GeneralizedApply extends LowGeneralizedApply {
  implicit def tupleApplies(x: GeneralizedApply): TupleApplies[x.arity.type] = new TupleApplies[x.arity.type](x.arity)
}

trait ArityApplies extends AnyRef {
  type Coll[N <: Nat, A]

  def apply[A](p1: A): Coll[_1, A]
  def apply[A](p1: A, p2: A): Coll[_2, A]
  def apply[A](p1: A, p2: A, p3: A): Coll[_3, A]
  def apply[A](p1: A, p2: A, p3: A, p4: A): Coll[_4, A]
  def apply[A](p1: A, p2: A, p3: A, p4: A, p5: A): Coll[_5, A]
  def apply[A](p1: A, p2: A, p3: A, p4: A, p5: A, p6: A): Coll[_6, A]
  def apply[A](p1: A, p2: A, p3: A, p4: A, p5: A, p6: A, p7: A): Coll[_7, A]
  def apply[A](p1: A, p2: A, p3: A, p4: A, p5: A, p6: A, p7: A, p8: A): Coll[_8, A]
  def apply[A](p1: A, p2: A, p3: A, p4: A, p5: A, p6: A, p7: A, p8: A, p9: A): Coll[_9, A]
  def apply[A](p1: A, p2: A, p3: A, p4: A, p5: A, p6: A, p7: A, p8: A, p9: A, ps: A*): Coll[??, A]
}

final class TupleApplies[AA <: ArityApplies](val arity: AA) extends AnyVal {
  import arity.Coll

  def apply[A](p: A): Coll[_1, A]                                 = arity[A](p)
  def apply[A](p: Tuple1[A]): Coll[_1, A]                         = arity[A](p._1)
  def apply[A](p: Tuple2[A, A]): Coll[_2, A]                      = arity[A](p._1, p._2)
  def apply[A](p: Tuple3[A, A, A]): Coll[_3, A]                   = arity[A](p._1, p._2, p._3)
  def apply[A](p: Tuple4[A, A, A, A]): Coll[_4, A]                = arity[A](p._1, p._2, p._3, p._4)
  def apply[A](p: Tuple5[A, A, A, A, A]): Coll[_5, A]             = arity[A](p._1, p._2, p._3, p._4, p._5)
  def apply[A](p: Tuple6[A, A, A, A, A, A]): Coll[_6, A]          = arity[A](p._1, p._2, p._3, p._4, p._5, p._6)
  def apply[A](p: Tuple7[A, A, A, A, A, A, A]): Coll[_7, A]       = arity[A](p._1, p._2, p._3, p._4, p._5, p._6, p._7)
  def apply[A](p: Tuple8[A, A, A, A, A, A, A, A]): Coll[_8, A]    = arity[A](p._1, p._2, p._3, p._4, p._5, p._6, p._7, p._8)
  def apply[A](p: Tuple9[A, A, A, A, A, A, A, A, A]): Coll[_9, A] = arity[A](p._1, p._2, p._3, p._4, p._5, p._6, p._7, p._8, p._9)
}
