package psp
package std

import Lookup._

/** This class separates the notion of having or not having a default from the
 *  actual lookup process. Failure to do this is the cause of many many bugs and
 *  unexpected behaviors in scala maps.
 */
object Lookup {
  sealed trait Default[-K, +V] extends (K => V) {
    def isEmpty = this == NoDefault
    def orElse[K1 <: K, V1 >: V](that: Default[K1, V1]): Default[K1, V1] = if (isEmpty) that else this
    def apply(x: K): V = this match {
      case NoDefault          => noSuchElementException(s"$x")
      case ConstantDefault(v) => v
      case FunctionDefault(f) => f(x)
    }
    def map[V1](g: V => V1): Default[K, V1] = this match {
      case NoDefault          => NoDefault
      case ConstantDefault(v) => ConstantDefault(g(v))
      case FunctionDefault(f) => FunctionDefault(f andThen g)
    }
    def comap[K1](g: K1 => K): Default[K1, V] = this match {
      case NoDefault          => NoDefault
      case ConstantDefault(v) => ConstantDefault(v)
      case FunctionDefault(f) => FunctionDefault(g andThen f)
    }
  }
  final case object NoDefault extends Default[Any, Nothing]
  final case class ConstantDefault[+V](value: V) extends Default[Any, V]
  final case class FunctionDefault[-K, +V](f: K => V) extends Default[K, V]

  def total[K, V](f: K => V): Lookup[K, V]   = new Lookup[K, V](f.partial, NoDefault)
  def apply[K, V](pf: K ?=> V): Lookup[K, V] = new Lookup[K, V](pf, NoDefault)
}

final class Lookup[K, V](pf: K ?=> V, fallback: Lookup.Default[K, V]) extends (K ?=> V) {
  def default                                                    = fallback
  def apply(key: K): V                                           = getOr(key, default(key))
  def comap[K1](f: K1 => K): Lookup[K1, V]                       = new Lookup(pf comap f, default comap f)
  def contains(key: K): Boolean                                  = pf isDefinedAt key
  def copmap[K1](pg: K1 ?=> K): Lookup[K1, V]                    = new Lookup(pf copmap pg, default comap pg)
  def get(key: K): Option[V]                                     = pf lift key
  def getOr[V1 >: V](key: K, alt: => V1): V1                     = if (isDefinedAt(key)) pf(key) else alt
  def isDefinedAt(key: K): Boolean                               = pf isDefinedAt key
  def map[V1](f: V => V1): Lookup[K, V1]                         = new Lookup(pf andThen f, default map f)
  def orElse(that: Lookup[K, V]): Lookup[K, V]                   = new Lookup(pf orElse that.pf, default orElse that.default)
  def put(key: K, value: V)(implicit z: HashEq[K]): Lookup[K, V] = new Lookup(exMap(key -> value).partial orElse pf, default)
  def withDefault(default: Default[K, V]): Lookup[K, V]          = new Lookup(pf, default)
}
