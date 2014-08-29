package psp
package std

object Show {
  def apply[A](f: A => String): Show[A] = new ShowClass(f)
  def native[A](): Show[A]              = ToString.castTo[Show[A]]

  /** This of course is not implicit as that would defeat the purpose of the endeavor.
   */
  val ToString: Show[Any] = apply[Any] {
    case x: ShowDirect => x.to_s
    case x             => "" + x
  }

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
