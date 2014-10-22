package psp
package std

import api._

object StdZero extends ZeroInstances
object StdEq extends EqInstances

object Unsafe {
  implicit def universalEq[A] : HashEq[A] = HashEq.natural()
  implicit def universalShow[A] : Show[A] = Show.natural()
}
