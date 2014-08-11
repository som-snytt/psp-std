package psp
package core

import scala.collection.{ mutable, immutable }

trait Extensional[+A] {
  def elements: Foreach[A]
  def map[B](f: A => B): Extensional[B]
}
trait Intensional[-A] {
  def contains: Predicate[A]
  def contraMap[B](f: B => A): Intensional[B]
}

// final class ExtensionalOps[A](val xs: Extensional[A]) extends AnyVal {
//   def mapOnto[B](f: A => B): PspMap[A, B] = PspMap(xs, f)
// }
// final class IntensionalOps[A](xs: Intensional[A]) extends AnyVal {
//   def mapOnto[B](f: A => B): Map[A, B] = Map(xs, f)
// }


final class IntensionalSet[-A](val contains: Predicate[A]) extends Intensional[A] {
  def contraMap[B](f: B => A): IntensionalSet[B] = new IntensionalSet[B]((x: B) => contains(f(x)))
}
final class ExtensionalSet[+A](val elements: Foreach[A]) extends Extensional[A] {
  def map[B](f: A => B): ExtensionalSet[B] = new ExtensionalSet(elements map f)
}

trait Map[-K, +V] {
  def contains(x: K): Boolean
  def apply(x: K): V
  def contraMap[K1](f: K1 => K): Map[K1, V]
  def map[V1](f: V => V1): Map[K, V1]
}
// trait PspMap[K, +V] extends Map[K, V] {
//   def keys: Extensional[K]
// }

// final class MappedPartialFunction[K, K1, V, V1](pf: K ?=> V, in: K1 => K, out: V => V1, default: K => V1) extends PartialFunction[K1, V1] {

//   def applyOrElse(x: K1, alt: K1 => V1): V = { val v = g(x) ; if (pf isDefinedAt v) pf(v) else alt(x) }
//   def isDefinedAt(x: K1): Boolean          = pf isDefinedAt g(x)
//   def apply(x: K1): V1                     = out(pf(in(x)))
// }

final class PspMap[K, V](pf: K ?=> V, default: K => V) extends Map[K, V] {
  def contains(x: K)                        = pf isDefinedAt x
  def apply(x: K): V                        = pf.applyOrElse(x, default)
  def applyOrElse(x: K, alt: K => V): V     = pf.applyOrElse(x, alt)
  def contraMap[K1](g: K1 => K): Map[K1, V] = PspMap[K1, V](pf contraMap g, g andThen default)
  def map[V1](f: V => V1): Map[K, V1]       = PspMap[K, V1](pf mapValues f, default andThen f)
}

object PspMap {
  def apply[K, V](pf: K ?=> V): PspMap[K, V]                  = apply(pf, x => throw new NoSuchElementException(s"$x"))
  def apply[K, V](pf: K ?=> V, default: K => V): PspMap[K, V] = new PspMap[K, V](pf, default)
}
object PspSet {
  def universal[A](xs: A*): EquivSet[A]           = EquivSet[A](Foreach.elems(xs: _*))(Equiv.universal[A])
  def reference[A <: AnyRef](xs: A*): EquivSet[A] = EquivSet[A](Foreach.elems(xs: _*))(Equiv.reference[A])

  def apply[Repr](xs: Repr)(implicit tc: Foreachable[Repr]) = new {
    def apply(containsFn: Predicate[tc.A]): PspSet[tc.A] = ???
  }
}

final class PspSet[A](val elements: Foreach[A], val contains: Predicate[A]) extends Intensional[A] with Extensional[A] {
  def map[B](f: A => B): Extensional[B]            = new ExtensionalSet(elements map f)
  def contraMap[B](f: B => A): Intensional[B]      = new IntensionalSet((x: B) => contains(f(x)))
  def withFilter(p: Predicate[A]): PspSet[A]       = new PspSet[A](elements filter p, x => contains(x) && p(x))
  def union(xs: PspSet[A])                         = new PspSet[A](elements ++ xs.elements, (x: A) => contains(x) || (xs contains x))
  def intersection(xs: IntensionalSet[A])          = new PspSet[A](elements filter xs.contains, (x: A) => contains(x) && (xs contains x))
  def remap[B](f: A => B): (PspSet[B], Foreach[A]) = {
    val jset    = jHashSet[B]
    val dropped = jList[A]
    elements filter contains foreach { x =>
      val y = f(x)
      if (jset contains y) dropped add x else jset add y
    }
    ((new PspSet(jset.toPsp, jset.contains), dropped.toPsp))
  }

  override def toString = s"Intensional Set"
}
object IntensionalSet {
  implicit def setToFunctionOne[A](xs: IntensionalSet[A]): Predicate[A] = xs.contains

  def apply[A](contains: Predicate[A]): IntensionalSet[A] = new IntensionalSet(contains)
  def apply[A](jSet: jHashSet[A]): IntensionalSet[A] = new IntensionalSet(jSet.contains)
  def apply[A](xs: Foreach[A]): IntensionalSet[A] = {
    val jset = new jHashSet[A]
    xs foreach jset.add
    apply(jset)
  }
}
