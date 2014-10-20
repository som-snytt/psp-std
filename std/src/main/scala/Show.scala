package psp
package std

import api._
import StdEq.stringEq
import psp.std.scalac.token.Keyword

/** When a type class is more trouble than it's worth.
 *  Not overriding toString here to leave open the possibility of
 *  using a synthetic toString, e.g. of case classes.
 */
trait ShowDirect extends Any { def to_s: String }

class TryShow[-A](shows: Show[A]) {
  def show(x: A): String = if (shows == null) "" + x else shows show x
}
object TryShow {
  implicit def apply[A](implicit z: Show[A] = Show.natural()): TryShow[A] = new TryShow[A](z)
}
final case class TryShown(to_s: String) extends AnyVal with ShowDirect {
  override def toString = to_s
}

/** Used to achieve type-safety in the show interpolator.
 *  It's the String resulting from passing a value through its Show instance.
 */
final case class Shown(to_s: String) extends AnyVal with ShowDirect {
  def ~ (that: Shown): Shown = new Shown(to_s + that.to_s)
  override def toString = to_s
}

object Shown {
  def empty: Shown             = new Shown("")
  def apply(ss: Shown*): Shown = if (ss.isEmpty) empty else ss reduceLeft (_ ~ _)
}

final class ShowDirectOps(val x: ShowDirect) extends AnyVal {
  /** Java-style String addition without abandoning type safety.
   */
  def + (that: ShowDirect): ShowDirect                = Shown(x.to_s + that.to_s)
  def + [A](that: A)(implicit z: Show[A]): ShowDirect = Shown(x.to_s + (z show that))
}

trait StdShow0 {
  // A weaker variation of Shown - use Show[A] if one can be found and toString otherwise.
  implicit def showableToTryShown[A](x: A)(implicit z: Show[A] = Show.natural()): TryShown = new TryShown(z show x)
}
trait StdShow1 extends StdShow0 {
  implicit def showableToShown[A](x: A)(implicit z: Show[A]): Shown = Shown(z show x)
}
trait StdShowLow extends StdShow1 {
  implicit def stringShow: Show[String]                                                 = Show(x => x)
  implicit def charShow: Show[Char]                                                     = Show.natural()
}

/** An incomplete selection of show compositors.
 *  Not printing the way scala does.
 */
trait StdShow extends StdShowLow {
  def inBrackets[A](xs: A*)(implicit shows: Show[A]): String      = xs map shows.show mkString ("[ ", ", ", " ]")
  implicit def attrNameShow : Show[java.util.jar.Attributes.Name] = Show.natural()
  implicit def exSetShow[A: Show] : Show[exSet[A]]                = showBy(_.contained)
  implicit def pMapShow[K: Show, V: Show] : Show[pMap[K, V]]      = Show(m => m.contained.tabular(_._1.to_s, _ => "->", _._2.to_s))
  implicit def pViewShow[A] : Show[View[A]]                       = Show(_.viewChain.reverse.map(_.description).joinWords.render) // map (_.description) joinWords)
  implicit def pListShow[A: Show] : Show[pList[A]]                = Show(xs => if (xs.isEmpty) "nil" else (xs join " :: ".asis) <> " :: nil" render)
  implicit def pVectorShow[A: Show] : Show[pVector[A]]            = Show(xs => if (xs.isEmpty) "[]" else inBrackets(xs.seq: _*)) // "[ " + ( xs map (_.to_s) mkString " " ) + " ]")

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

  implicit def stackTraceElementShow: Show[StackTraceElement] = Show("\tat" + _ + "\n")

  implicit def intensionalSetShow[A: Show] : Show[IntensionalSet[A]] = {
    import IntensionalSet._
    Show[IntensionalSet[A]] {
      case xs: ExtensionalSet[A] => show"${xs.contained}"
      case Filtered(lhs, rhs)    => show"Filtered($lhs, ${rhs.toString})"
      case Complement(xs)        => show"Not($xs)"
      case Intersect(lhs, rhs)   => show"Intersect($lhs, $rhs)"
      case Union(lhs, rhs)       => show"Union($lhs, $rhs)"
      case Diff(lhs, rhs)        => show"Diff($lhs, $rhs)"
      case Impl(member, heq)     => pp"Impl($member, $heq)"
    }
  }

  implicit def keywordShow: Show[Keyword] = Show[Keyword] {
    case Keyword.Empty            => ""
    case Keyword.ClassConstructor => ""
    case Keyword.ValueParameter   => ""
    case Keyword.TypeParameter    => ""
    case Keyword.Constructor      => "def this"
    case Keyword.PackageObject    => "package object"
    case Keyword.CaseObject       => "case object"
    case Keyword.CaseClass        => "case class"
    case k                        => k.toString.toLowerCase
  }

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
    case PreciseSize(size)     => show"$size"
    case Bounded(lo, Infinite) => show"$lo+"
    case Bounded(lo, hi)       => "[%s,%s]".format(lo.toString, hi.toString)
    case Infinite              => "<inf>"
  }

  // case Foreach.Unfold(zero)              => show"unfold from $zero"
  // case Foreach.Joined(xs, ys)            => show"$xs ++ $ys"
  // case Foreach.Constant(elem)            => show"Constant($elem)"
  // case Foreach.Continually(fn)           => show"Continually(<fn>)"
  // case Foreach.KnownSize(Infinite)       => "<inf>"
  // case Foreach.KnownSize(PreciseSize(0)) => "[]"
  // case xs @ Foreach.KnownSize(size: PreciseSize) =>
    // case xs: IndexRange                    => xs.toString

  private def showElems[A: Show](small: PreciseSize, large: Int, xs: sciList[A]): String = xs splitAt large match {
    case (Nil, _)  => "[]"
    case (xs, Nil) => xs.m.joinComma.surround("[ ", " ]").render
    case (xs, _)   => (xs.m take small joinComma).surround("[ ", ", ... ]").render
  }
  implicit def pSeqShow[A: Show] : Show[pSeq[A]] = Show[pSeq[A]] { xs =>
    val small = 3.size
    val large = 10
    def elems() = showElems[A](small, large - 2, xs.m take large toScalaList)
    (xs, xs.sizeInfo) match {
      case ( xs: IndexRange, _)                                => s"$xs"
      case ( _: Direct[_] | _: Linear[_], _)                   => elems()
      case (Foreach.Joined(xs, ys), _)                         => show"$xs ++ $ys"
      case (_, Bounded(PreciseSize(0L), Infinite))             => show"${xs.shortClass}"
      case (_, Bounded(PreciseSize(n), Infinite)) if n < large => show"${xs.shortClass} (size $n+)"
      case _                                                   => elems()
    }
  }
}

final class ShowInterpolator(val x: StringContext) extends AnyVal {
  /** The type of args forces all the interpolation variables to
   *  be of a type which is implicitly convertible to Shown, which
   *  means they have a Show[A] in scope.
   */
  def show(args: Shown*): String  = StringContext(x.parts: _*).raw(args: _*)
  def pp(args: TryShown*): String = StringContext(x.parts: _*).raw(args: _*)
  def shown(args: Shown*): Shown  = Shown(show(args: _*))
  def doc(args: Doc*): Doc        = (x.parts.m.map(_.asis) intersperse args.seq.m).joinChars

  /** Can't see any way to reuse the standard (type-safe) f-interpolator, will
   *  apparently have to reimplement it entirely.
   */
  def fshow(args: Shown*): String = x.parts.mkString("").format(args: _*)
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
  def show(arg: TryShown, maxElements: Int): String = {
    val s = arg.to_s
    val nl = if (s contains "\n") "\n" else ""
    nl + s + "\n"
  }
}
