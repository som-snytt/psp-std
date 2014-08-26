package psp
package std

/** The classic type class for turning string representations into typed values.
 */
trait Read[A] extends Any {
  def read(s: String): A
}

/** For this to have any hope of being smooth, we need the VALUE
 *  (the type class instance) to be inferred, but the TYPE
 *  to be given explicitly. Type inference can't do anything sensible
 *  if the only incoming type is a String.
 *
 *  That's what into[A] is for, to obtain the type up front.
 */
object Read {
  implicit val bigIntRead = Read[BigInt](s => BigInt(s))
  implicit val bigDecRead = Read[BigDecimal](s => BigDecimal(s))
  implicit val stringRead = Read[String](s => s)
  implicit val floatRead  = Read[Float](_.toFloat)
  implicit val doubleRead = Read[Double](_.toDouble)
  implicit val longRead   = Read[Long](_.toLong)
  implicit val intRead    = Read[Int](_.toInt)

  def apply[A](f: String => A): Read[A]                         = ReadClass(f)
  def unapply[A](s: String)(implicit reads: Read[A]): Option[A] = Try(reads read s).toOption
  def into[A] = new ReadInto[A]

  final class ReadInto[A]() {
    def apply(s: String)(implicit reads: Read[A]): A           = reads read s
    def unapply(s: String)(implicit reads: Read[A]): Option[A] = opt(s)
    def wrap(s: String)(implicit reads: Read[A]): Try[A]       = Try(reads read s)
    def opt(s: String)(implicit reads: Read[A]): Option[A]     = wrap(s).toOption
  }
  private case class ReadClass[A](f: String => A) extends AnyVal with Read[A] { def read(s: String): A = f(s) }
}
