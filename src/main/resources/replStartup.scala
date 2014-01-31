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
val s = """
def map[B](f: A => B): MapTo[B]
def flatMap[B](f: A => Foreach[B]): MapTo[B]
def collect[B](pf: A =?> B): MapTo[B]
def withFilter(p: Predicate[A]): MapTo[A]
def filter(p: Predicate[A]): MapTo[A]
def filterNot(p: Predicate[A]): MapTo[A]
def drop(n: Int): MapTo[A]
def take(n: Int): MapTo[A]
def dropRight(n: Int): MapTo[A]
def takeRight(n: Int): MapTo[A]
def slice(start: Int, end: Int): MapTo[A]
def slice(range: Interval): MapTo[A]
def labeled(label: String): MapTo[A]
def sized(size: Size): MapTo[A]
def reverse: MapTo[A]
"""
val xs = s.toLines
