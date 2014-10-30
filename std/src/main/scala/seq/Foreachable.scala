package psp
package std

import api._

trait Foreachable[Repr] {
  type Elem
  def wrap(repr: Repr): AtomicView[Elem, Repr]
  // def build(xs: View[Elem]): Repr
}
trait ForeachableLinear[Repr] extends Foreachable[Repr] {
  def wrap(repr: Repr): LinearView[Elem, Repr]
}
trait ForeachableIndexed[Repr] extends Foreachable[Repr] {
  def wrap(repr: Repr): DirectView[Elem, Repr]
}
trait ForeachableSet[Repr] extends Foreachable[Repr] {
  def wrap(repr: Repr): ExSetView[Elem, Repr]
}

abstract class ForeachableCompanion[CC[X] <: Foreachable[X]] {
  type Coll[A, Repr] = CC[Repr] { type Elem = A }
}

object Foreachable extends ForeachableCompanion[Foreachable] {
  def apply[A, Repr](f: Repr => Each[A]): Coll[A, Repr] = new Foreachable[Repr] {
    type Elem = A
    def wrap(repr: Repr): AtomicView[Elem, Repr] = new LinearView[A, Repr](f(repr))
  }

  implicit def PolicyForeachIs[A, CC[X] <: Each[X]] : Coll[A, CC[A]]       = apply[A, CC[A]](xs => xs)
  implicit def ScalaCollectionIs[A, CC[X] <: sCollection[X]] : Coll[A, CC[A]] = apply[A, CC[A]](fromScala)
  implicit def JavaIterableIs[A, CC[X] <: jIterable[X]] : Coll[A, CC[A]]      = apply[A, CC[A]](fromJava)
}

object ForeachableSet extends ForeachableCompanion[ForeachableSet] {
  def apply[A, Repr](f: Repr => ExSet[A]): Coll[A, Repr] = new ForeachableSet[Repr] {
    type Elem = A
    def wrap(repr: Repr) = new ExSetView(f(repr))
  }
  implicit def PolicySetIs[A, CC[X] <: ExSet[X]] : Coll[A, CC[A]] = apply[A, CC[A]](xs => xs)
  implicit def JavaSetIs[A, CC[X] <: jSet[X]] : Coll[A, CC[A]]    = apply[A, CC[A]](xs => new PolicySet.FromJava(xs))
  implicit def ScalaSetIs[A, CC[X] <: scSet[X]] : Coll[A, CC[A]]  = apply[A, CC[A]](xs => new PolicySet.FromScala(xs.toSet))
  implicit def ScalaBitSetIs: Coll[Int, sciBitSet]                = apply[Int, sciBitSet](xs => new PolicySet.FromScala(xs))
}

object ForeachableLinear extends ForeachableCompanion[ForeachableLinear] {
  def apply[A, Repr](f: Repr => Each[A]): Coll[A, Repr] = new ForeachableLinear[Repr] {
    type Elem = A
    def wrap(repr: Repr): LinearView[A, Repr] = new LinearView[A, Repr](f(repr))
  }
  implicit def PolicyLinearIs[A, CC[X] <: Linear[X]] : Coll[A, CC[A]]             = apply[A, CC[A]](xs => xs)
  implicit def ScalaSeqIs[A, CC[X] <: scSeq[X]] : Coll[A, CC[A]]                  = apply[A, CC[A]](fromScala)
  implicit def ScalaMapIs[K, V, CC[X, Y] <: scMap[X, Y]] : Coll[(K, V), CC[K, V]] = apply[(K, V), CC[K, V]](fromScala)

}
object ForeachableIndexed extends ForeachableCompanion[ForeachableIndexed] {
  def apply[A, Repr](f: Repr => Direct[A]): Coll[A, Repr] = new ForeachableIndexed[Repr] {
    type Elem = A
    def wrap(repr: Repr): DirectView[A, Repr] = new DirectView[A, Repr](f(repr))
  }

  implicit def ArrayIs[A] : Coll[A, Array[A]]                                  = apply[A, Array[A]](Direct fromArray _)
  implicit def JavaStringIs: Coll[Char, String]                                = apply[Char, String](Direct fromString _)
  implicit def PolicyDirectIs[A, CC[X] <: Direct[X]] : Coll[A, CC[A]]          = apply[A, CC[A]](xs => xs)
  implicit def ScalaIndexedSeqIs[A, CC[X] <: scIndexedSeq[A]] : Coll[A, CC[A]] = apply[A, CC[A]](xs => new Direct.FromScala(xs.toIndexedSeq))
}
