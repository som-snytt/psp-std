package psp
package std

sealed trait Walkable[-Repr] {
  type CC[X]
  type A
  def foreach(repr: Repr)(f: A => Unit): Unit
}
trait Foreachable[-Repr] extends Walkable[Repr] {
  def sizeInfo(repr: Repr): SizeInfo = unknownSize
  def wrap[R <: Repr](repr: R): AtomicView[R, this.type] = AtomicView.unknown(repr)(this)
}
trait DirectAccess[-Repr] extends Foreachable[Repr] {
  def length(repr: Repr): Size
  def elemAt(repr: Repr)(index: Index): A
  override def sizeInfo(repr: Repr): SizeInfo = Precise(length(repr))
  override def wrap[R <: Repr](repr: R): IndexedView[R, this.type] = AtomicView.indexed(repr)(this)
}

object Foreachable {
  final class ForeachIs[AIn] extends Foreachable[Foreach[AIn]] {
    type CC[X] = Foreach[X]
    type A = AIn
    def foreach(repr: Foreach[A])(f: A => Unit): Unit = repr foreach f
  }
  final class TraversableIs[AIn] extends Foreachable[Traversable[AIn]] {
    type CC[X] = Traversable[X]
    type A = AIn
    def foreach(repr: CC[A])(f: A => Unit): Unit = repr foreach f
  }
  final class ArrayIs[AIn] extends Foreachable[Array[AIn]] {
    type CC[X] = Array[A]
    type A = AIn
    def foreach(repr: CC[A])(f: A => Unit): Unit = {
      val len = repr.length
      var i = 0
      while (i < len) {
        f(repr(i))
        i += 1
      }
    }
  }

  implicit def foreachIs[A] : ForeachIs[A]         = new ForeachIs[A]
  implicit def traversableIs[A] : TraversableIs[A] = new TraversableIs[A]
  implicit def arrayIs[A] : ArrayIs[A]             = new ArrayIs[A]
}

object DirectAccess {
  trait Impl[AIn, Repr, M[X]] extends DirectAccess[Repr] {
    @inline final def foreach(repr: Repr)(f: A => Unit): Unit = length(repr) foreachIndex (i => f(elemAt(repr)(i)))
    type CC[X] = M[X]
    type A     = AIn
  }
  object StringIs extends Impl[Char, String, Direct] {
    def length(repr: String): Size               = Size(repr.length)
    def elemAt(repr: String)(index: Index): Char = repr charAt index.value
  }
  final class ArrayIs[A] extends Impl[A, Array[A], Direct] {
    def length(repr: Array[A]): Size            = Size(repr.length)
    def elemAt(repr: Array[A])(index: Index): A = repr(index.value)
  }
  final class ScalaIndexedIs[A] extends Impl[A, IndexedSeq[A], IndexedSeq] {
    def length(repr: CC[A]): Size            = Size(repr.length)
    def elemAt(repr: CC[A])(index: Index): A = repr(index)
  }
  final class IndexedIs[AIn] extends Impl[AIn, Direct[AIn], Direct] {
    def length(repr: Direct[A]): Size            = repr.size
    def elemAt(repr: Direct[A])(index: Index): A = repr elemAt index
  }

  implicit def stringIs: StringIs.type               = StringIs
  implicit def indexedIs[A] : IndexedIs[A]           = new IndexedIs[A]
  implicit def arrayIs[A] : ArrayIs[A]               = new ArrayIs[A]
  implicit def scalaIndexedIs[A] : ScalaIndexedIs[A] = new ScalaIndexedIs[A]
}

object Has {
  trait ApplyType         extends Any                { type In ; type Out         }
  trait ElemType          extends Any                { type Elem                  }
  trait CcType            extends Any                { type CC[X]                 }
  trait ApplyParams[I, O] extends Any with ApplyType { type In = I ; type Out = O }
  trait ElemParam[A]      extends Any with ElemType  { type Elem = A              }
  trait CcParam[M[X]]     extends Any with CcType    { type CC[X] = M[X]          }

  trait Tail[R]      extends Any with ElemType    { def hasTail(xs: R): R                                   }
  trait Head[-R]     extends Any with ElemType    { def hasHead(xs: R): Elem                                }
  trait IsEmpty[-R]  extends Any                  { def hasIsEmpty(xs: R): Boolean                          }
  trait SizeInfo[-R] extends Any with ElemType    { def hasSizeInfo(xs: R): std.SizeInfo                    }
  trait Size[-R]     extends Any with ElemType    { def hasSize(xs: R): std.Size                            }
  trait Contains[-R] extends Any with ElemType    { def hasContains(xs: R)(x: Elem): Boolean                }
  trait Foreach[-R]  extends Any with ElemType    { def hasForeach(xs: R)(f: Elem => Unit): Unit            }
  trait Filter[R]    extends Any with ElemType    { def hasFilter(xs: R)(p: Predicate[Elem]): R             }
  trait Index[-R]    extends Any with ElemType    { def hasIndex(xs: R)(index: std.Index): Elem             }
  trait View[R]      extends Any with ElemType    { def hasView(xs: R): api.View[Elem]                      }
  trait Apply[-R]    extends Any with ApplyType   { def hasApply(xs: R)(in: In): Out                        }
  trait Map[R]       extends Any with Is.Packaged { def hasMap[A](xs: R)(f: Elem => A): CC[A]               }
  trait FlatMap[R]   extends Any with Is.Packaged { def hasFlatMap[A](xss: R)(f: Elem => Foreach[A]): CC[A] }
}

object Is {
  trait Packaged extends Any with Has.CcType with Has.ElemType

  trait Container[M[X], A]       extends Any with Has.CcParam[M]     with Has.ElemParam[A]
  trait IndexContainer[M[X], A]  extends Any with Is.Container[M, A] with Is.Index[M[A]]
  trait LinearContainer[M[X], A] extends Any with Is.Container[M, A] with Is.Linear[M[A]]
  trait ForeachView[M[X], A]     extends Any with Has.Foreach[M[A]]  with Has.View[M[A]] with Has.SizeInfo[M[A]]
  trait Iterable[-R]             extends Any with Has.Foreach[R]     with Has.SizeInfo[R]
  trait Seq[-R]                  extends Any with Has.Foreach[R]
  trait Set[-R]                  extends Any with Has.Foreach[R]     with Has.Contains[R]
  trait Map[-R]                  extends Any with Has.Foreach[R]     with Has.Contains[R]  with Has.Apply[R]  { type Elem = In }
  trait Index[R]                 extends Any with Is.Seq[R]          with Has.Index[R]     with Has.Size[R]
  trait Linear[R]                extends Any with Is.Seq[R]          with Has.Head[R]      with Has.Tail[R] with Has.IsEmpty[R]

  abstract class Array[A] extends IndexContainer[Array, A]
}
