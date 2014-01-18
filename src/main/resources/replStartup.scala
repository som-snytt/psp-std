import scala.collection.{ mutable, immutable }
import scala.reflect.runtime.{ universe => ru }
import psp._, core._, linear._, view._
import immutable.BitSet
implicit final class ReplOps(val target: Any) {
  def >(): Unit = show()
  def show(): Unit = psp.core show target
}
val lin = Foreach.to(1, 100).psp
val ind = Indexed.to(1, 100).psp
def announce[T](x: T): T = try x finally println(x)
