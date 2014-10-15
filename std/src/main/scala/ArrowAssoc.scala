package psp
package std

import ArrowAssoc.Types

/** Hand specialized on the left, @specialized on the right, value classes for tuple creation.
 */
final class ArrowAssocInt(val self: Int) extends AnyVal {
  @inline def -> [@specialized(Types) B](y: B): Tuple2[Int, B] = Tuple2(self, y)
}
final class ArrowAssocLong(val self: Long) extends AnyVal {
  @inline def -> [@specialized(Types) B](y: B): Tuple2[Long, B] = Tuple2(self, y)
}
final class ArrowAssocDouble(val self: Double) extends AnyVal {
  @inline def -> [@specialized(Types) B](y: B): Tuple2[Double, B] = Tuple2(self, y)
}
final class ArrowAssocChar(val self: Char) extends AnyVal {
  @inline def -> [@specialized(Types) B](y: B): Tuple2[Char, B] = Tuple2(self, y)
}
final class ArrowAssocBoolean(val self: Boolean) extends AnyVal {
  @inline def -> [@specialized(Types) B](y: B): Tuple2[Boolean, B] = Tuple2(self, y)
}
final class ArrowAssocRef[A](val self: A) extends AnyVal {
  @inline def -> [B](y: B): Tuple2[A, B] = Tuple2(self, y)
}

trait StdArrowAssoc0 {
  // Lower priority than the hand-specialized variations.
  @inline final implicit def arrowAssocRef[A](x: A): ArrowAssocRef[A] = new ArrowAssocRef(x)
}

trait StdArrowAssoc extends StdArrowAssoc0 {
  @inline final implicit def arrowAssocInt(x: Int): ArrowAssocInt             = new ArrowAssocInt(x)
  @inline final implicit def arrowAssocLong(x: Long): ArrowAssocLong          = new ArrowAssocLong(x)
  @inline final implicit def arrowAssocDouble(x: Double): ArrowAssocDouble    = new ArrowAssocDouble(x)
  @inline final implicit def arrowAssocChar(x: Char): ArrowAssocChar          = new ArrowAssocChar(x)
  @inline final implicit def arrowAssocBoolean(x: Boolean): ArrowAssocBoolean = new ArrowAssocBoolean(x)
}

object ArrowAssoc {
  val Types = new scala.Specializable.Group((scala.Int, scala.Long, scala.Double, scala.Char, scala.Boolean))
}
