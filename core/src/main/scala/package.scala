package psp

import scala.{ collection => sc }
import sc.{ mutable, immutable, generic }
import psp.common._

package object core extends ScalaTypes with PspTypes with MidPriorityPsp {
  type Precise = SizeInfo.Precise

  def nullAs[A] : A = null.asInstanceOf[A]

  def Zero                     = Size.Zero
  def precise(n: Int): Precise = SizeInfo.Precise(Size(n))

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
    def nTimes(n: Int): Foreach[T] = Foreach.times(n, x)
    def toRef: AnyRef = castTo[AnyRef]
    def ref_==(y: Any): Boolean = x.toRef eq y.toRef

    def mk_s(sep: String): String = x match {
      case xs: Foreach[_]     => xs.toList map (_.to_s) mkString sep
      case xs: Traversable[_] => xs map (_.to_s) mkString sep
      case _                  => to_s
    }
    def to_s: String = x match {
      case x: Labeled               => x.label
      case _: PartialFunction[_, _] => "<pf>"
      case _: Function1[_, _]       => "<f>"
      case _                        => "" + x
    }
    def pp: String = x match {
      case xs: Foreach[_] => "%15s  %s".format(xs.sizeInfo.to_s, xs.to_s)
      case _              => x.to_s
    }
    def shortClass: String = decodeName(x.getClass.getName split "[.]" last)
  }

  implicit class ToSInterpolatorOps(val stringContext: StringContext) {
    final def ss(args: Any*): String = StringContext(stringContext.parts: _*).s(args map (_.to_s): _*)
  }

  implicit class PspViewEnter[A](val xs: Foreach[A]) {
    def m: PspView[A]    = PspView(xs)
    def view: PspView[A] = PspView(xs)
  }
  implicit class PspViewExit[A](val xs: PspView[A]) {
    def force[That](implicit cb: CBF[Nothing, A, That]): That = {
      val buf = cb()
      xs foreach (buf += _)
      buf.result
    }
  }

  implicit final class SizeInfoOps(val lhs: SizeInfo) extends AnyVal {
    import PartialOrder._
    import SizeInfo._
    import ThreeValue._

    def isZero = lhs == precise(0)
    def atMost: SizeInfo  = bounded(Zero, lhs)
    def atLeast: SizeInfo = bounded(lhs, Infinite)

    // def +(amount: Size): Precise = Precise(size + amount)
    // def -(amount: Size): Precise = Precise(size - amount)

    def * (rhs: Size): SizeInfo = lhs match {
      case Precise(n)               => Precise(n * rhs.value)
      case Bounded(lo, Precise(hi)) => Bounded(lo * rhs.value, Precise(hi * rhs.value))
      case Bounded(lo, _)           => if (rhs.isZero) Unknown else Bounded(lo * rhs.value, Infinite)
      case Infinite                 => if (rhs.isZero) Unknown else Infinite
    }

    def + (rhs: SizeInfo): SizeInfo = (lhs, rhs) match {
      case (Infinite, _) | (_, Infinite)            => Infinite
      case (Precise(l), Precise(r))                 => Precise(l + r)
      case (GenBounded(l1, h1), GenBounded(l2, h2)) => bounded(l1 + l2, h1 + h2)
    }
    def - (rhs: SizeInfo): SizeInfo = (lhs, rhs) match {
      case (Infinite, Finite(_, _))         => Infinite
      case (Finite(_, _), Infinite)         => Empty
      case (Finite(l1, h1), Finite(l2, h2)) => bounded(l1 - h2, Precise(h1 - l2))
      case (Bounded(l1, h1), Precise(n))    => bounded(l1 - n, h1 - Precise(n))
      case _                                => Unknown
    }
    def min(rhs: SizeInfo): SizeInfo = lhs partialCompare rhs match {
      case LT | LE | EQ => lhs
      case GT | GE      => rhs
      case _            => onBounds(rhs)((l1, h1, l2, h2) => bounded(l1 min l2, h1 min h2))
    }
    def max(rhs: SizeInfo): SizeInfo = lhs partialCompare rhs match {
      case LT | LE | EQ => rhs
      case GT | GE      => lhs
      case _            => onBounds(rhs)((l1, h1, l2, h2) => bounded(l1 max l2, h1 max h2))
    }

    private def onBounds[T](rhs: SizeInfo)(f: (Size, Atomic, Size, Atomic) => T): T = {
      val GenBounded(l1, h1) = lhs
      val GenBounded(l2, h2) = rhs

      f(l1, h1, l2, h2)
    }
  }

}
