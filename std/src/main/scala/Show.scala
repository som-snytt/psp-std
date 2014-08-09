package psp
package std

/** The classic type class for encoding string representations.
 */
trait Show[A] {
  def show(x: A): String
}
object Show {
  def apply[A](f: A => String): Show[A] = new Show[A] { def show(x: A): String = f(x) }

  /** I believe this is the first time I have ever been able to
   *  use an AnyVal bound other than when demonstrating bugs.
   */
  implicit def stringShow: Show[String]                        = Show(x => x)
  implicit def optShow[A: Show] : Show[Option[A]]              = Show(_.fold("None")("Some(" + _.to_s + ")"))
  implicit def seqShow[CC[X] <: Seq[X], A: Show] : Show[CC[A]] = Show(xs => xs map (_.to_s) mkString (xs.stringPrefix + "(", ", ", ")"))
  implicit def arrayShow[A: Show] : Show[Array[A]]             = Show(_ map (_.to_s) mkString ("Array(", ", ", ")"))
  implicit def anyValShow[A <: AnyVal] : Show[A]               = Show("" + _)

  final class Ops[A](val x: A) extends AnyVal {
    def to_s(implicit shows: Show[A]): String = shows show x
  }
}

/** Used to achieve type-safety in the show interpolator.
 */
final class Shown[A: Show](val value: A) {
  override def toString = value.to_s
}
final class ShowInterpolator(val sc: StringContext) extends AnyVal {
  /** The type of args forces all the interpolation variables to
   *  be of a type which is implicitly convertible to Shown, which
   *  means they have a Show[A] in scope.
   */
  def show(args: Shown[_]*): String = new StringContext(sc.parts: _*).raw(args: _*)
}
