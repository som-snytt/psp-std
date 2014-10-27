package psp
package std

import api._
import StdEq.stringEq
import StdZero._

/** When a type class is more trouble than it's worth.
 *  Not overriding toString here to leave open the possibility of
 *  using a synthetic toString, e.g. of case classes.
 */
trait ShowDirect extends Any { def to_s: String }
trait ForceShowDirect extends Any with ShowDirect {
  override def toString = to_s
}

class TryShow[-A](shows: Show[A]) {
  def show(x: A): String = if (shows == null) "" + x else shows show x
}
object TryShow {
  implicit def apply[A](implicit z: Show[A] = Show.natural()): TryShow[A] = new TryShow[A](z)
}
final case class TryShown(try_s: String) extends AnyVal {
  override def toString = try_s
}

/** Used to achieve type-safety in the show interpolator.
 *  It's the String resulting from passing a value through its Show instance.
 */
final case class Shown(to_s: String) extends AnyVal with ForceShowDirect {
  def ~ (that: Shown): Shown = new Shown(to_s + that.to_s)
}

object Shown {
  def empty: Shown             = new Shown("")
  def apply(ss: Shown*): Shown = ss.m safeReduce (_ ~ _)
}

final class ShowDirectOps(val x: ShowDirect) extends AnyVal {
  /** Java-style String addition without abandoning type safety.
   */
  def + (that: ShowDirect): ShowDirect                = Shown(x.to_s + that.to_s)
  def + [A](that: A)(implicit z: Show[A]): ShowDirect = Shown(x.to_s + (z show that))
}

final class ShowInterpolator(val x: StringContext) extends AnyVal {
  /** The type of args forces all the interpolation variables to
   *  be of a type which is implicitly convertible to Shown, which
   *  means they have a Show[A] in scope.
   */
  def show(args: Shown*): String  = StringContext(x.parts: _*).raw(args: _*)
  def pp(args: TryShown*): String = StringContext(x.parts: _*).raw(args: _*)
  def shown(args: Shown*): Shown  = Shown(show(args: _*))
  def doc(args: Doc*): Doc        = (Generator(x.parts.m map (_.asis)) intersperse Generator(args.m)).view.joinChars

  /** Can't see any way to reuse the standard (type-safe) f-interpolator, will
   *  apparently have to reimplement it entirely.
   */
  def fshow(args: Shown*): String = (x.parts map (_.processEscapes) mkString "").format(args: _*)
}

object Show {
  final class Impl[-A](val f: Shower[A]) extends AnyVal with Show[A] { def show(x: A) = f(x) }

  def apply[A](f: Shower[A]): Show[A] = new Impl[A](f)
  def natural[A](): Show[A]           = ToString

  /** This of course is not implicit as that would defeat the purpose of the endeavor.
   */
  private object ToString extends Show[Any] {
    def show(x: Any): String = x match {
      case null          => ""
      case x: ShowDirect => x.to_s
      case x             => x.toString
    }
    override def toString = "ToString"
  }
}

/** For this to have any hope of being smooth, we need the VALUE
 *  (the type class instance) to be inferred, but the TYPE
 *  to be given explicitly. Type inference can't do anything sensible
 *  if the only incoming type is a String.
 *
 *  That's what into[A] is for, to obtain the type up front.
 */
object Read {
  def apply[A](f: String => A): Read[A]                         = new Impl[A](f)
  def unapply[A](s: String)(implicit reads: Read[A]): Option[A] = Try(reads read s).toOption
  def into[A] : ReadInto[A]                                     = new ReadInto[A]

  final class ReadInto[A]() {
    def apply(s: String)(implicit reads: Read[A]): A           = reads read s
    def unapply(s: String)(implicit reads: Read[A]): Option[A] = opt(s)
    def wrap(s: String)(implicit reads: Read[A]): Try[A]       = Try(reads read s)
    def opt(s: String)(implicit reads: Read[A]): Option[A]     = wrap(s).toOption
  }

  final class Impl[A](val f: String => A) extends AnyVal with Read[A] { def read(x: String): A = f(x) }
}

package object repl {
  def show(arg: TryShown, maxElements: Int): String = {
    val s = arg.try_s
    val nl = if (s contains "\n") "\n" else ""
    nl + s + "\n"
  }
}
