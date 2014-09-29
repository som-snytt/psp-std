package psp
package std

import api._

/** When a type class is more trouble than it's worth.
 *  Not overriding toString here to leave open the possibility of
 *  using a synthetic toString, e.g. of case classes.
 *  ShowDirectNow performs that override.
 */
trait ShowDirect extends Any { def to_s: String }
trait ShowDirectNow extends Any with ShowDirect { final override def toString = to_s }
trait TryShow[-A] extends Any { def show(x: A): String }

/** Used to achieve type-safety in the show interpolator.
 *  It's the String resulting from passing a value through its Show instance.
 */
final case class Shown(to_s: String) extends AnyVal with ShowDirectNow
final case class TryShown(to_s: String) extends AnyVal with ShowDirectNow

final class ShowDirectOps(val __psp_x: ShowDirect) extends AnyVal {
  /** Java-style String addition without abandoning type safety.
   */
  def + (that: ShowDirect): ShowDirect = Shown(__psp_x.to_s + that.to_s)
  def + [A: Show](that: A): ShowDirect = Shown(__psp_x.to_s + (implicitly[Show[A]] show that))
}

/** An incomplete selection of show compositors.
 *  Not printing the way scala does.
 */
trait ShowImplicits extends LowShowImplicits {
  def inBrackets[A](xs: A*)(implicit shows: Show[A]): String = xs map shows.show mkString ("[", ", ", "]")

  implicit def viewShow[A] : Show[View[A]]             = Show(_.viewChain reverseMap (_.description) joinSpace)
  implicit def pspListShow[A: Show] : Show[PspList[A]] = Show(xs => if (xs.isEmpty) "nil" else (xs join " :: ") + " :: nil")

  implicit def booleanShow: Show[Boolean]               = Show.native()
  implicit def charShow: Show[Char]                     = Show.native()
  implicit def doubleShow: Show[Double]                 = Show.native()
  implicit def intShow: Show[Int]                       = Show.native()
  implicit def longShow: Show[Long]                     = Show.native()
  implicit def numberShow: Show[scala.math.ScalaNumber] = Show.native()
  implicit def showDirect: Show[ShowDirect]             = Show(_.to_s)
  implicit def stringShow: Show[String]                 = Show(x => x)

  implicit def indexShow: Show[api.Index] = new Ops.ShowBy[api.Index] apply (_.value.toString)
  implicit def sizeShow: Show[Size]       = new Ops.ShowBy[Size] apply (_.value.toString)

  implicit def arrayShow[A: Show] : Show[Array[A]]        = Show(xs => inBrackets(xs: _*))
  implicit def optShow[A: Show] : Show[Option[A]]         = Show(_.fold("-")(implicitly[Show[A]].show))
  implicit def seqShow[A: Show] : Show[Seq[A]]            = Show(xs => inBrackets(xs: _*))
  implicit def tupleShow[A: Show, B: Show] : Show[(A, B)] = Show { case (x, y) => "%s -> %s".format(implicitly[Show[A]] show x, implicitly[Show[B]] show y) }

  implicit def sizeInfoShow: Show[SizeInfo] = Show[SizeInfo] {
    case Bounded(lo, Infinite) => "[%s, <inf>)".format(lo.toString)
    case Bounded(lo, hi)       => "[%s, %s]".format(lo.toString, hi.toString)
    case Precise(size)         => size.toString
    case Infinite              => "<inf>"
  }
}

trait LowShowImplicits {
  // A weaker variation of Shown - use Show[A] if one can be found and toString otherwise.
  implicit def showableToTryShown[A](x: A)(implicit shows: TryShow[A]): TryShown = new TryShown(shows show x)
}

final class ShowInterpolator(val __psp_sc: StringContext) extends AnyVal {
  /** The type of args forces all the interpolation variables to
   *  be of a type which is implicitly convertible to Shown, which
   *  means they have a Show[A] in scope.
   */
  def show(args: Shown*): String  = StringContext(__psp_sc.parts: _*).raw(args: _*)
  def shown(args: Shown*): Shown  = Shown(show(args: _*))
  def pp(args: TryShown*): String = StringContext(__psp_sc.parts: _*).raw(args: _*)
}

object Show {
  final class Impl[-A](val f: A => String) extends AnyVal with Show[A] { def show(x: A) = f(x) }
  def apply[A](f: A => String): Show[A] = new Impl[A](f)
  def native[A](): Show[A]              = ToString

  /** This of course is not implicit as that would defeat the purpose of the endeavor.
   */
  private val ToString = apply[Any]({
    case null          => ""
    case x: ShowDirect => x.to_s
    case x             => x.toString
  })
}

trait LowTryShow {
  self: TryShow.type =>

  implicit def hasNoShow[A] : TryShow[A] = NoShow
}
object TryShow extends LowTryShow {
  final class HasShow[-A](shows: Show[A]) extends TryShow[A] { def show(x: A) = shows show x }
  final object NoShow extends TryShow[Any] { def show(x: Any): String = "" + x }

  implicit def hasShow[A](implicit shows: Show[A]): HasShow[A] = new HasShow(shows)
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

