package psp

import scala.{ collection => sc }
import sc.{ mutable, immutable, generic }
import psp.common._
import psp.view._

package core {
  trait LowPriorityPsp {
    implicit def implicitForeachView[CC[X] <: Foreach[X], A](xs: CC[A]): LinearId[A] = ViewOp linear xs

    // This implicit is for "Reprs" like String and Array
    implicit def mkIndirectViewOps[Coll](xs: Coll)(implicit itl: IsTraversableLike[Coll]): View[Coll, itl.A] =
    new View[Coll, itl.A](xs)(xs => (itl conversion xs).seq.toTraversable)
    // implicit def mkIndirectViewOps[Coll](xs: Coll)(implicit itl: IsTraversableLike[Coll]): ReprId[Coll, itl.A] = ViewOp.repr(xs)

      // (Foreach traversable (itl conversion xs).seq.toTraversable).psp
    // : View[Coll, itl.A] =
    // new View[Coll, itl.A](xs)(xs => (itl conversion xs).seq.toTraversable)
  }

  trait MidPriorityPsp extends LowPriorityPsp {
    implicit def mkPartiallyOrderOps[A](x: PartiallyOrdered[A]): PartiallyOrderedOps[A]                  = new PartiallyOrderedOps(x)
    // This implicit is for collections like Seq[A]
    implicit def mkDirectViewOps[Coll, A](xs: Coll)(implicit ev: Coll <:< Traversable[A]): View[Coll, A] = new View[Coll, A](xs)(ev)
    // implicit def mkDirectViewOps[Coll, A](xs: Coll)(implicit ev: Coll <:< Traversable[A]): ViewOp[Foreach, A] = (Foreach traversable xs).psp

    // implicit def mkDirectViewOps[Coll, A](xs: Coll)(implicit ev: Coll <:< Traversable[A]): ViewOp[Foreach, A] = (Foreach traversable xs).psp
    // new View[Coll, A](xs)(ev)
  }
}

package object core extends ScalaTypes with PspTypes with MidPriorityPsp {
  type Precise = SizeInfo.Precise
  type PspList[A] = psp.core.linear.PspList[A]
  val PspList = psp.core.linear.PspList

  def nullAs[A] : A = null.asInstanceOf[A]

  def Zero                     = Size.Zero
  def precise(n: Int): Precise = SizeInfo.Precise(Size(n))

  def companionOf[CC[X] <: Traversable[X]](xs: CC[_]): GenericCompanion[CC] = xs.companion.castTo[GenericCompanion[CC]]

  def failEmpty(operation: String): Nothing = throw new NoSuchElementException(s"$operation on empty collection")

  def andTrue(x: Unit): Boolean = true
  def andFalse(x: Unit): Boolean = false

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
      case xs: Viewable[_]    => Foreach[Any](xs foreach _) mk_s sep
      case _                  => to_s
    }
    def to_s: String = x match {
      case x: psp.common.Labeled    => x.label
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
  implicit def implicitIndexedView[CC[X] <: Indexed[X], A](xs: CC[A]): IndexedId[A] = ViewOp indexed xs
  implicit def implicitSizeInfoOps(info: SizeInfo): SizeInfoOperations = new SizeInfoOperations(info)
}
