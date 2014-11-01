package psp
package std

import api._

object StdZero extends ZeroInstances
object StdEq extends EqInstances
object StdMonoid extends MonoidInstances
object StdShow extends ShowInstances

object Unsafe extends LowPriorityUnsafe {
  /** This would be great for string literals only. */
  implicit def stringToDoc(s: String): Doc = Doc(s)

  implicit def universalEq[A] : HashEq[A]        = HashEq.natural()
  implicit def universalShow[A] : Show[A]        = Show.natural()
  implicit def showableOrder[A: Show] : Order[A] = orderBy[A](_.to_s)
}
trait LowPriorityUnsafe {
  // We may as well derive some convenience from the absence of parametricity.
  implicit def universalOrder[A] : Order[A] = orderBy[A](x => ("" + _, x.##))
}
