package psp
package std
package ops

import api._

abstract class PartialImpl[-T, +R](contains: T => Boolean, f: T => R) extends (T ?=> R) {
  def isDefinedAt(x: T) = contains(x)
  def apply(x: T): R    = f(x)
}
final class FunctionAsPartial[-T, +R](f: T => R)                 extends PartialImpl[T, R](true, f)
final class PairPartial[-T : Eq, +R](key: T, value: R)           extends PartialImpl[T, R](key === _, _ => value)
final class JoinPartial[-T, +R](pf1: T ?=> R, pf2: T ?=> R)      extends PartialImpl[T, R](x => (pf1 contains x) && (pf2 contains x), x => (pf1 lift x) | pf2(x))
final class ComapPartial[T1, -T, +R](pf: T ?=> R, g: T1 => T)    extends PartialImpl[T1, R](x => pf contains g(x), x => pf(g(x)))
final class CopmapPartial[T1, -T, +R](pf: T ?=> R, pg: T1 ?=> T) extends PartialImpl[T1, R](x => (pg contains x) && (pf contains pg(x)), x => pf(pg(x)))

final class MapFunction1[A, B, C](f: A => B, g: B => C) extends (A => C) { def apply(x: A): C = g(f(x)) }

/** "Extensions" are classes which only exist to add methods to
 *  built-in types from the scala standard library. As we phase
 *  out the use of the standard library these will migrate into
 *  "Ops" classes, where we control the underlying class.
 */
final class Function1Ops[T, R](val f: T => R) extends AnyVal {
  def |:(label: String): LabeledFunction[T, R] = new LabeledFunction(f, label)
  def :|(label: String): LabeledFunction[T, R] = new LabeledFunction(f, label)

  def untupled[A, B](implicit z: PairUp[T, A, B]): (A, B) => R = (x, y) => f(z.create(x, y))

  def partial: T ?=> R                                   = new FunctionAsPartial(f)
  def map[S](g: R => S): T => S                          = new MapFunction1(f, g)
  def comap[S](g: S => T): S => R                        = new MapFunction1(g, f)
  def sameAt(g: T => R)(implicit z: Eq[R]): Predicate[T] = x => f(x) === g(x)
  def on[S](g: (R, R) => S): (T, T) => S                 = (x, y) => g(f(x), f(y))
}
final class Function2Ops[T1, T2, R](val f: (T1, T2) => R) extends AnyVal {
  def tupled: ((T1, T2)) => R              = xy => f(xy._1, xy._2)
  def andThen[S](g: R => S): (T1, T2) => S = (x, y) => g(f(x, y))
  def map[S](g: R => S): (T1, T2) => S     = (x, y) => g(f(x, y))
  def comap1[P](g: P => T1): (P, T2) => R  = (x, y) => f(g(x), y)
  def comap2[P](g: P => T2): (T1, P) => R  = (x, y) => f(x, g(y))
}
final class BiFunctionOps[T, R](val f: (T, T) => R) extends AnyVal {
  // def on[S](g: S => T): (S, S) => R = (x, y) => f(g(x), g(y))
}
final class PredicateOps[A](val p: Predicate[A]) extends AnyVal {
  def inSet: InSet[A] = InSet(p)
}
final class PartialFunctionOps[A, B](val pf: A ?=> B) extends AnyVal {
  def contains(x: A)                        = pf isDefinedAt x
  def comap[A1](f: A1 => A): A1 ?=> B       = new ComapPartial(pf, f)
  def copmap[A1](pg: A1 ?=> A): A1 ?=> B    = new CopmapPartial(pf, pg)
  def zapply(x: A)(implicit z: Empty[B]): B = if (pf isDefinedAt x) pf(x) else z.empty
}

final class OptionOps[A](val x: Option[A]) extends AnyVal {
  def | (alt: => A): A                             = x getOrElse alt
  def ||(alt: => A): Option[A]                     = x orElse Some(alt)
  def |?[A1 >: A](alt: => A1): A1                  = x getOrElse alt
  def ||?[A1 >: A](alt: => Option[A1]): Option[A1] = x orElse alt

  def or(alt: => A): A       = x getOrElse alt
  def orFail(msg: String): A = x getOrElse abort(msg)
  def toDirect: Direct[A]    = Direct fromScala x.toVector
}

final class TryOps[A](val x: Try[A]) extends AnyVal {
  def | (expr: => A): A = x match {
    case Failure(_) => expr
    case Success(x) => x
  }
  def || (expr: => A): Try[A] = x match {
    case x @ Success(_) => x
    case Failure(_)     => Try(expr)
  }
  def fold[B](f: Throwable => B, g: A => B): B = x match {
    case Success(x) => g(x)
    case Failure(t) => f(t)
  }
}

/*** Java ***/

final class JavaIteratorOps[A](it: jIterator[A]) {
  def foreach(f: A => Unit): Unit = while (it.hasNext) f(it.next)
}

final class FileTimeOps(val time: jFileTime) extends AnyVal {
  def isNewer(that: jFileTime) = (time compareTo that) > 0
  def isOlder(that: jFileTime) = (time compareTo that) < 0
  def isSame(that: jFileTime)  = (time compareTo that) == 0
}

final class ClassLoaderOps(val loader: jClassLoader) extends ClassLoaderTrait
