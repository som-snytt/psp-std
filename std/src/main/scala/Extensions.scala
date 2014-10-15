package psp
package std
package ops

import api.Eq

/** "Extensions" are classes which only exist to add methods to
 *  built-in types from the scala standard library. As we phase
 *  out the use of the standard library these will migrate into
 *  "Ops" classes, where we control the underlying class.
 */
final class PredicateOps[A](val f: Predicate[A]) extends AnyVal {
  def ^(g: Predicate[A]): Predicate[A] = x => f(x) ^ g(x)
}
final class Function1Ops[T, R](val f: T => R) extends AnyVal {
  def comap[S](g: S => T): S => R = g andThen f
  def sameAt(g: T => R)(implicit eqs: Eq[R]): Predicate[T] = x => f(x) === g(x)
}

final class OptionOps[A](val x: Option[A]) extends AnyVal {
  def | (alt: => A): A            = x getOrElse alt
  def ||(alt: => A): Option[A]    = x orElse Some(alt)

  def |?[A1 >: A](alt: => A1): A1                  = x getOrElse alt
  def ||?[A1 >: A](alt: => Option[A1]): Option[A1] = x orElse alt
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
final class ClassOps(val c: jClass) extends AnyVal {
  def shortName = decodeName(c.getName.dottedSegments.last)
}
