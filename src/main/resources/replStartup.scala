import scala.collection.{ mutable, immutable }
import scala.reflect.runtime.{ universe => ru }
import psp._, core._, linear._
import immutable.BitSet
implicit final class ReplOps(val target: Any) {
  def >(): Unit = show()
  def show(): Unit = psp.core show target
}
