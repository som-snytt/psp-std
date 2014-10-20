package psp
package std

sealed trait IsDefault[+A] { def value: A }
final class DefaultFunction[+A](valueFn: => A) extends IsDefault[A] { def value = valueFn }
final class DefaultValue[+A](val value: A) extends IsDefault[A]

final case class Classpath(value: String) extends AnyVal { override def toString = value }
final case class Version(value: String) extends AnyVal   { override def toString = value }
