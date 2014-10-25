package psp
package std
package ops

import api._

/** "Extensions" are classes which only exist to add methods to
 *  built-in types from the scala standard library. As we phase
 *  out the use of the standard library these will migrate into
 *  "Ops" classes, where we control the underlying class.
 */
final class Function1Ops[T, R](val f: T => R) extends AnyVal {
  def |:(label: String): LabeledFunction[T, R] = new LabeledFunction(f, label)
  def :|(label: String): LabeledFunction[T, R] = new LabeledFunction(f, label)

  def untupled[A, B](implicit z: PairUp[T, A, B]): (A, B) => R = (x, y) => f(z.create(x, y))

  def map[S](g: R => S): T => S                          = f andThen g
  def comap[S](g: S => T): S => R                        = g andThen f
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
  def inSet(implicit z: HashEq[A]): inSet[A] = IntensionalSet(p)
}
final class PartialFunctionOps[A, B](val pf: A ?=> B) extends AnyVal {
  def comap[A1](f: A1 => A): A1 ?=> B    = newPartial(x => pf isDefinedAt f(x), x => pf(f(x)))
  def copmap[A1](pg: A1 ?=> A): A1 ?=> B = newPartial(x => (pg isDefinedAt x) && (pf isDefinedAt pg(x)), x => pf(pg(x)))
}

final class OptionOps[A](val x: Option[A]) extends AnyVal {
  def | (alt: => A): A                             = x getOrElse alt
  def ||(alt: => A): Option[A]                     = x orElse Some(alt)
  def |?[A1 >: A](alt: => A1): A1                  = x getOrElse alt
  def ||?[A1 >: A](alt: => Option[A1]): Option[A1] = x orElse alt

  def orFail(msg: String): A = x getOrElse abort(msg)
  def pvec: pVector[A] = if (x.isEmpty) Direct() else Direct(x.get)
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
  def fold[B](f: A => B, g: Throwable => B): B = x match {
    case Success(x) => f(x)
    case Failure(t) => g(t)
  }
}

/*** Java ***/

final class FileTimeOps(val time: jFileTime) extends AnyVal {
  def isNewer(that: jFileTime) = (time compareTo that) > 0
  def isOlder(that: jFileTime) = (time compareTo that) < 0
  def isSame(that: jFileTime)  = (time compareTo that) == 0
}

final class ClassLoaderOps(val loader: jClassLoader) extends ClassLoaderTrait
