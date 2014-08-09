package psp
package std

trait Show[A] {
  def show(x: A): String
}
object Show {
  def apply[A](f: A => String): Show[A] = new Show[A] { def show(x: A): String = f(x) }

  implicit def stringShow: Show[String]                        = Show(x => x)
  implicit def optShow[A: Show] : Show[Option[A]]              = Show(_.fold("None")("Some(" + _.to_s + ")"))
  implicit def seqShow[CC[X] <: Seq[X], A: Show] : Show[CC[A]] = Show(xs => xs map (_.to_s) mkString (xs.stringPrefix + "(", ", ", ")"))
  implicit def arrayShow[A: Show] : Show[Array[A]]             = Show(_ map (_.to_s) mkString ("Array(", ", ", ")"))
  implicit def anyValShow[A <: AnyVal] : Show[A]               = Show("" + _)
}

final class Shown[A: Show](val value: A) {
  override def toString = value.to_s
}

final class ShowInterpolator(val sc: StringContext) extends AnyVal {
  def show(args: Shown[_]*): String = new StringContext(sc.parts: _*).raw(args: _*)
}
