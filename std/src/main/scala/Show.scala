package psp
package std

/** The classic type class for encoding string representations.
 */
trait Show[A] extends Any {
  def show(x: A): String
}

/** When a type class is more trouble than it's worth.
 *  Not overriding toString here to leave open the possibility of
 *  using a synthetic toString, e.g. of case classes.
 */
trait ShowDirect extends Any {
  def to_s: String
}

trait LowPriorityShow {
  implicit def intShow     = Show.native[Int]()
  implicit def longShow    = Show.native[Long]()
  implicit def doubleShow  = Show.native[Double]()
  implicit def booleanShow = Show.native[Boolean]()
  implicit def charShow    = Show.native[Char]()
}
object Show extends LowPriorityShow {
  def apply[A](f: A => String): Show[A] = new ShowClass(f)
  def native[A](): Show[A]              = ToString.castTo[Show[A]]

  /** This of course is not implicit as that would defeat the purpose of the endeavor.
   */
  val ToString: Show[Any] = apply[Any] {
    case x: ShowDirect => x.to_s
    case x             => "" + x
  }

  def inBrackets[A: Show](xs: A*): String = xs map (_.to_s) mkString ("[", ", ", "]")

  /** An incomplete selection of show compositors.
   *  Not printing the way scala does.
   */
  implicit def stringShow: Show[String]                        = apply(x => x)
  implicit def optShow[A: Show] : Show[Option[A]]              = apply(_.fold("-")(x => show"$x"))
  implicit def seqShow[CC[X] <: Seq[X], A: Show] : Show[CC[A]] = apply(xs => inBrackets(xs: _*))
  implicit def arrayShow[A: Show] : Show[Array[A]]             = apply(xs => inBrackets(xs: _*))
  implicit def tupleShow[A: Show, B: Show] : Show[(A, B)]      = apply { case (x, y) => show"$x -> $y" }
  implicit def showDirect[A <: ShowDirect] : Show[A]           = native[A]()
  implicit def numberShow[A <: ScalaNumber] : Show[A]          = native[A]() // BigInt, BigDecimal

  private class ShowClass[A](private val f: A => String) extends AnyVal with Show[A] {
    def show(x: A): String = f(x)
  }
}

object Shown {
  def apply(s: String): Shown = new Shown(s)
}

/** Used to achieve type-safety in the show interpolator.
 *  It's the String resulting from passing a value through its Show instance.
 */
final class Shown(val to_s: String) extends AnyVal with ShowDirect {
  override def toString = to_s
}
final class TryShown(val to_s: String) extends AnyVal with ShowDirect {
  override def toString = to_s
}
final class ShowInterpolator(val sc: StringContext) extends AnyVal {
  /** The type of args forces all the interpolation variables to
   *  be of a type which is implicitly convertible to Shown, which
   *  means they have a Show[A] in scope.
   */
  def show(args: Shown*): String  = StringContext(sc.parts: _*).raw(args: _*)
  def pp(args: TryShown*): String = StringContext(sc.parts: _*).raw(args: _*)
}
