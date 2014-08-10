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
  /** I believe this is the first time I have ever been able to
   *  use an AnyVal bound other than when demonstrating bugs.
   */
  implicit def anyValShow[A <: AnyVal] : Show[A] = Show.native[A]()
}
object Show extends LowPriorityShow {
  def apply[A](f: A => String): Show[A] = ShowClass(f)
  def native[A](): Show[A]              = ToString.castTo[Show[A]]

  /** This of course is not implicit as that would defeat the purpose of the endeavor.
   */
  val ToString: Show[Any] = ShowClass[Any] {
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

  implicit def showDirect[A <: ShowDirect] : Show[A]  = native[A]()
  implicit def numberShow[A <: ScalaNumber] : Show[A] = native[A]() // BigInt, BigDecimal

  private case class ShowClass[A](f: A => String) extends AnyVal with Show[A] { def show(x: A): String = f(x) }
}

object ShowDirect {
  implicit class ShowDirectOps(val x: ShowDirect) extends AnyVal {
    /** Java-style String addition without abandoning type safety.
     */
    def + (that: ShowDirect): ShowDirect = ShowDirect(show"$x$that")
    def + [A: Show](that: A): ShowDirect = ShowDirect(show"$x$that")
  }
  def apply(s: String): ShowDirect = new Shown(s)
}

/** Used to achieve type-safety in the show interpolator.
 *  It's the String resulting from passing a value through its Show instance.
 */
final class Shown(val to_s: String) extends AnyVal with ShowDirect {
  override def toString = to_s
}
final class ShowInterpolator(val sc: StringContext) extends AnyVal {
  /** The type of args forces all the interpolation variables to
   *  be of a type which is implicitly convertible to Shown, which
   *  means they have a Show[A] in scope.
   */
  def show(args: Shown*): String = new StringContext(sc.parts: _*).raw(args: _*)
}
