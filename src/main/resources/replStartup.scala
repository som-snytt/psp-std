import scala.collection.{ mutable, immutable }
import scala.reflect.runtime.{ universe => ru }
import psp._, core._
import immutable.BitSet
implicit final class ReplOps[A](val target: A) {
  def >(): Unit = target.toElements map (_.to_s) foreach println
  def show(): Unit = println(target.to_s)
}
def announce[T](x: T): T = try x finally println(x)
val lin = PspList.to(1, 20).m
val ind = IntRange.to(1, 20).m
def stats() { println(s"lin: ${lin.calls}, ind: ${ind.calls}") }
