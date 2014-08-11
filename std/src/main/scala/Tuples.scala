package psp
package std

/** Hand specialized on the left, @specialized on the right, value classes for tuple creation.
 */
final class ArrowAssocInt(private val self: Int) extends AnyVal {
  @inline def -> [@specialized(Int, Long, Double, Char, Boolean) B](y: B): Tuple2[Int, B] = Tuple2(self, y)
}
final class ArrowAssocLong(private val self: Long) extends AnyVal {
  @inline def -> [@specialized(Int, Long, Double, Char, Boolean) B](y: B): Tuple2[Long, B] = Tuple2(self, y)
}
final class ArrowAssocDouble(private val self: Double) extends AnyVal {
  @inline def -> [@specialized(Int, Long, Double, Char, Boolean) B](y: B): Tuple2[Double, B] = Tuple2(self, y)
}
final class ArrowAssocChar(private val self: Char) extends AnyVal {
  @inline def -> [@specialized(Int, Long, Double, Char, Boolean) B](y: B): Tuple2[Char, B] = Tuple2(self, y)
}
final class ArrowAssocBoolean(private val self: Boolean) extends AnyVal {
  @inline def -> [@specialized(Int, Long, Double, Char, Boolean) B](y: B): Tuple2[Boolean, B] = Tuple2(self, y)
}
final class ArrowAssocRef[A](private val self: A) extends AnyVal {
  @inline def -> [B](y: B): Tuple2[A, B] = Tuple2(self, y)
}


trait ArrowAssocLow {
  @inline final implicit def arrowAssocRef[A](x: A): ArrowAssocRef[A] = new ArrowAssocRef(x)
}
trait ArrowAssocHigh extends ArrowAssocLow {
  @inline final implicit def arrowAssocInt(x: Int): ArrowAssocInt             = new ArrowAssocInt(x)
  @inline final implicit def arrowAssocLong(x: Long): ArrowAssocLong          = new ArrowAssocLong(x)
  @inline final implicit def arrowAssocDouble(x: Double): ArrowAssocDouble    = new ArrowAssocDouble(x)
  @inline final implicit def arrowAssocChar(x: Char): ArrowAssocChar          = new ArrowAssocChar(x)
  @inline final implicit def arrowAssocBoolean(x: Boolean): ArrowAssocBoolean = new ArrowAssocBoolean(x)
}
