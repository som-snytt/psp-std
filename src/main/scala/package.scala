package psp

import scala.{ collection => sc }
import sc.{ mutable, immutable, generic }
import psp.compat._
import psp.core.impl._

package core {
  trait LowPriorityPsp {
    @inline final implicit def pspIntWrapper(x: Int): PspInt                  = new PspInt(x)
    @inline final implicit def implicitUniversalOps[T](x: T): UniversalOps[T] = new UniversalOps(x)
  }

  trait MidPriorityPsp extends LowPriorityPsp {
    implicit def implicitLinearView[Coll](repr: Coll)(implicit tc: Foreachable[Coll]): LinearView[Coll, tc.CC, tc.A] = LinearView(repr)
    implicit def implicitSetView[Coll](repr: Coll)(implicit tc: ForeachableSet[Coll]): SetView[Coll, tc.CC, tc.A]    = SetView(repr)
    implicit def implicitPspStringOps(s: String): PspStringOps                                                       = new PspStringOps(s)
    implicit def lowerPspStringOps(s: PspStringOps): String                                                          = s.repr
    implicit def implicitPartiallyOrderOps[A](x: PartiallyOrdered[A]): PartiallyOrderedOps[A]                        = new PartiallyOrderedOps(x)
  }

  trait HighPriorityPsp extends MidPriorityPsp {
    implicit def implicitIndexedView[Coll](repr: Coll)(implicit tc: Indexable[Coll]): IndexedView[Coll, tc.CC, tc.A] = IndexedView(repr)
    implicit def implicitPartialFunctionOps[T, R](pf: T =?> R): PartialFunctionOps[T, R]                             = new PartialFunctionOps[T, R](pf)
    implicit def implicitFunctionOps[T, R](f: T => R): Function1Ops[T, R]                                            = new Function1Ops[T, R](f)
    implicit def implicitArrayOps[A](xs: Array[A]): PspArrayOps[A]                                                   = new PspArrayOps[A](xs)
    implicit def implicitPpInterpolatorOps(sc: StringContext): PpInterpolatorOps                                     = new PpInterpolatorOps(sc)
  }

  class Counter() {
    private[this] var counted = 0
    def inc(): this.type = try this finally counted += 1
    def count: Int = counted
    def record[T](x: T): T = try x finally inc()
  }

  trait GenericUtility {
    def join(sep: String)(xs: Any*): String = xs mkString sep
    def andTrue(x: Unit): Boolean           = true
    def andFalse(x: Unit): Boolean          = false
    def nullAs[A] : A                       = (null: AnyRef).castTo[A]
    def decodeName(s: String): String       = scala.reflect.NameTransformer.decode(s)

    def timed[A](body: => A): A = {
      val start = System.nanoTime
      try body finally log("Elapsed: %.3f ms" format (System.nanoTime - start) / 1e6)
    }
  }
}

package object core extends PspLogging with GenericUtility with ScalaTypes with JavaTypes with PspTypes with HighPriorityPsp with ScalaShadowImplicits {
  type Precise = SizeInfo.Precise
  // type PspList[A] = psp.core.linear.PspList[A]
  // val PspList = psp.core.linear.PspList

  def labelpf[T, R](label: String)(pf: T =?> R): T =?> R = new LabeledPartialFunction(pf, label)
  def classTag[T: ClassTag] = implicitly[ClassTag[T]]
  def newBuffer[A](): ArrayBuffer[A] = mutable.ArrayBuffer[A]()

  def Zero                     = Size.Zero
  def precise(n: Int): Precise = SizeInfo.Precise(Size(n))
  def failEmpty(operation: String): Nothing = throw new NoSuchElementException(s"$operation on empty collection")
}

