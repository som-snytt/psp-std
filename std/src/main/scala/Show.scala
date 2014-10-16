package psp
package std

import api._
import StdEq.stringEq

/** When a type class is more trouble than it's worth.
 *  Not overriding toString here to leave open the possibility of
 *  using a synthetic toString, e.g. of case classes.
 *  ShowDirectNow performs that override.
 */
trait ShowDirect extends Any { def to_s: String }
trait ShowDirectNow extends Any with ShowDirect { final override def toString = to_s }

/** Used to achieve type-safety in the show interpolator.
 *  It's the String resulting from passing a value through its Show instance.
 */
final case class Shown(to_s: String) extends AnyVal with ShowDirectNow {
  def ~ (that: Shown): Shown = new Shown(to_s + that.to_s)
}
final case class TryShown(to_s: String) extends AnyVal with ShowDirectNow

object Shown {
  def empty: Shown             = new Shown("")
  def apply(ss: Shown*): Shown = if (ss.isEmpty) empty else ss reduceLeft (_ ~ _)
}

final class ShowDirectOps(val __psp_x: ShowDirect) extends AnyVal {
  /** Java-style String addition without abandoning type safety.
   */
  def + (that: ShowDirect): ShowDirect = Shown(__psp_x.to_s + that.to_s)
  def + [A: Show](that: A): ShowDirect = Shown(__psp_x.to_s + (?[Show[A]] show that))
}

/** An incomplete selection of show compositors.
 *  Not printing the way scala does.
 */
trait StdShow extends StdShowLow {
  def inBrackets[A](xs: A*)(implicit shows: Show[A]): String = xs map shows.show mkString ("[ ", ", ", " ]")
  // implicit def pMapShow[K: Show, V: Show] : Show[pMap[K, V]] = Show(m => m.tabular(_._1.to_s, _ => "->", _._2.to_s))
  implicit def pViewShow[A] : Show[View[A]]                  = Show(_.viewChain.pvec.reverse map (_.description) joinSpace)
  implicit def pListShow[A: Show] : Show[pList[A]]           = Show(xs => if (xs.isEmpty) "nil" else (xs join " :: ") + " :: nil")
  implicit def pVectorShow[A: Show] : Show[pVector[A]]       = Show(xs => if (xs.isEmpty) "[]" else inBrackets(xs.seq: _*)) // "[ " + ( xs map (_.to_s) mkString " " ) + " ]")

  implicit def booleanShow: Show[Boolean]               = Show.natural()
  implicit def doubleShow: Show[Double]                 = Show.natural()
  implicit def intShow: Show[Int]                       = Show.natural()
  implicit def longShow: Show[Long]                     = Show.natural()
  implicit def numberShow: Show[scala.math.ScalaNumber] = Show.natural()
  implicit def showDirect: Show[ShowDirect]             = Show(_.to_s)
  implicit def showClass: Show[jClass]                  = Show(_.shortName)
  implicit def indexShow: Show[Index]                   = showBy[Index](_.indexValue)
  implicit def nthShow: Show[Nth]                       = showBy[Nth](_.nthValue)

  implicit def arrayShow[A: Show] : Show[Array[A]]        = Show(xs => inBrackets(xs: _*))
  implicit def optShow[A: Show] : Show[Option[A]]         = Show(_.fold("-")(?[Show[A]].show))
  implicit def seqShow[A: Show] : Show[Seq[A]]            = Show(xs => inBrackets(xs: _*))
  implicit def tupleShow[A: Show, B: Show] : Show[(A, B)] = Show { case (x, y) => show"$x -> $y" }

  //  Show {
  //   case null                  => "<null>"
  //   case _: jWildcardType      => "_"
  //   case x: jTypeVariable[_]   => x.getName
  //   case x: jParameterizedType => x.constructor.to_s + x.args.m.optBrackets
  //   case x: jGenericArrayType  => show"Array[${x.getGenericComponentType}]"
  //   case x: jClass             => x.shortName
  // }

  // implicit def jMethodShow: Show[jMethod] = Show { m =>
  //   val ts = m.typeParams.m.optBrackets
  //   val ps = m.paramTypes mapWithNth ((nth, tp) => show"p$nth: $tp") joinComma
  //   val rs = m.returnType
  //   // m stripPackage
  //   show"def ${m.name}$ts($ps): $rs"
  // }

  implicit def sizeInfoShow: Show[SizeInfo] = Show[SizeInfo] {
    case Bounded(lo, Infinite) => "[%s, <inf>)".format(lo.toString)
    case Bounded(lo, hi)       => "[%s, %s]".format(lo.toString, hi.toString)
    case PreciseSize(size)     => size.toString
    case Infinite              => "<inf>"
  }

  private def stringify[A: Show](xs: Foreach[A], max: Int = 3): String = {
    def base = xs.m take max joinComma;
    xs.sizeInfo match {
      case PreciseSize(0)             => pp"[ ]"
      case PreciseSize(n) if n <= max => pp"[ $base ]"
      case PreciseSize(n)             => pp"[ $base, ... $n elements ]"
      case Infinite                   => pp"[ $base, ... <inf> ]"
      case info                       => pp"[ $base, ... $info ]"
    }
  }

  implicit def showForeach[A: Show] : Show[Foreach[A]] = Show[Foreach[A]] {
    case Foreach.Unfold(zero)      => show"unfold from $zero"
    case Foreach.Join(xs, ys)      => show"$xs ++ $ys"
    case Foreach.Constant(elem)    => show"Constant($elem)"
    case Foreach.Continually(fn)   => show"Continually(<fn>)"
    case Foreach.Times(size, elem) => show"$elem x" + (sizeInfoShow show size)
    case xs                        => stringify[A](xs)
  }
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
  def natural[A](): Show[A]             = ToString

  /** This of course is not implicit as that would defeat the purpose of the endeavor.
   */
  private val ToString = apply[Any]({
    case null          => ""
    case x: ShowDirect => x.to_s
    case x             => x.toString
  })
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

object StdShow extends StdShow {
  def apply[A: Show] : Show[A] = implicitly[Show[A]]
}


package object repl {
  def show[A: TryShow](arg: A, maxElements: Int): String = {
    val s = implicitly[TryShow[A]] show arg
    val nl = if (s contains "\n") "\n" else ""
    nl + s + "\n"
  }
}
