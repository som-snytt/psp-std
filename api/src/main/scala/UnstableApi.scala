package psp
package std
package api

import scala.Any
import scala.Int
import java.lang.String

trait Hash[-A] extends Any { def hash(x: A): Int }
trait TryHash[-A] extends Any { def hash(x: A): Int }

/** A way of externalizing a String attachment on types.
 *  Used for example to attach a String to a function instance.
 *  There is a fallback no-op instance in the companion scope.
 *  TODO - should there be?
 */
trait Labelable[A] {
  def label(value: A, label: String): A
}
object Labelable {
  class LabelableIdentity[A] extends Labelable[A] {
    def label(value: A, label: String): A = value
  }
  implicit def labelIdentity[A] : Labelable[A] = new LabelableIdentity[A]
}
