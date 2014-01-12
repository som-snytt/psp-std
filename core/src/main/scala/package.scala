package psp

import scala.{ collection => sc }
import sc.{ mutable, immutable, generic }
import psp.common._

package object core extends ScalaTypes with PspTypes with MidPriorityPsp {
  type Precise = SizeInfo.Precise

  def Zero                                = Size.Zero
  def precise(n: Int): Precise            = SizeInfo.Precise(Size(n))
  def bounded(lo: Int, hi: Int): SizeInfo = SizeInfo.bounded(Size(lo), precise(hi))

  def companionOf[CC[X] <: Traversable[X]](xs: CC[_]): GenericCompanion[CC] = xs.companion.castTo[GenericCompanion[CC]]

  def failEmpty(operation: String): Nothing = throw new NoSuchElementException(s"$operation on empty collection")

  def trunc(x: Any, max: Int): String = {
    val s = x.to_s
    if (s.length <= max) s else (s take max - 3) + "..."
  }

  def log(msg: String): Unit = Console.err println msg
  def printResult[A](msg: String)(x: A): A  = try x finally Console.err.println(ss"$msg: $x")
  def logResult[A](x: A): A  = try x finally log(ss"$x")
  def logError[A](msg: String)(x: A): A = {
    (new Throwable).getStackTrace take 10 foreach println
    try x finally log(ss"$msg: $x")
  }

  def decodeName(s: String): String = scala.reflect.NameTransformer.decode(s)

  def timed[A](body: => A): A = {
    val start = System.nanoTime
    try body finally println("Elapsed: %.3f ms".format((System.nanoTime - start) / 1e6))
  }

  def show(target: Any): Unit = target match {
    case xs: Foreach[_]     => xs foreach println
    case xs: Traversable[_] => xs foreach (x => println(x))
    case x                  => println(x)
  }

  implicit class AnyOps[T](val x: T) extends AnyVal {
    def castTo[U] : U = x.asInstanceOf[U]
    def toRef: AnyRef = castTo[AnyRef]
    def ref_==(y: Any): Boolean = x.toRef eq y.toRef

    def mk_s(sep: String): String = x match {
      case xs: Foreach[_]                 => xs.toList map (_.to_s) mkString sep
      case xs: Traversable[_]             => xs map (_.to_s) mkString sep
      case _                              => to_s
    }
    def to_s: String = x match {
      case x: Labeled => x.label
      case _          => "" + x
    }
    def shortClass: String = decodeName(x.getClass.getName split "[.]" last)
  }

  implicit class ToSInterpolatorOps(val stringContext: StringContext) {
    final def ss(args: Any*): String = StringContext(stringContext.parts: _*).s(args map (_.to_s): _*)
  }
}
