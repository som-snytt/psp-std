package psp
package std

trait WalkableTypes {
  type A
  type CC[X]
}

sealed trait Walkable[-Repr] extends WalkableTypes {
  def foreach(repr: Repr)(f: A => Unit): Unit
}
trait Foreachable[-Repr] extends Walkable[Repr] {
  def sizeInfo(repr: Repr): SizeInfo = unknownSize

  def wrap[R <: Repr](repr: R): AtomicView[R, this.type] = AtomicView.unknown(repr)(this)
}
trait SequentialAccess[Repr] extends Walkable[Repr] {
  def head(repr: Repr): A
  def tail(repr: Repr): Repr
  def isEmpty(repr: Repr): Boolean

  def wrap(repr: Repr): LinearView[Repr, this.type] = AtomicView.linear(repr)(this)
}
trait DirectAccess[-Repr] extends Walkable[Repr] {
  def length(repr: Repr): Size
  def elemAt(repr: Repr)(index: Index): A

  def wrap[R <: Repr](repr: R): IndexedView[R, this.type] = AtomicView.indexed(repr)(this)
}

object Foreachable {
  trait Impl[A0, Repr, CC0[X]] extends Foreachable[Repr] {
    type CC[X] = CC0[X]
    type A     = A0
  }
  final class ForeachIs[A] extends Impl[A, Foreach[A], Foreach] {
    def foreach(repr: Foreach[A])(f: A => Unit): Unit = repr foreach f
  }
  final class TraversableIs[CC[X] <: Traversable[X], A] extends Impl[A, CC[A], CC] {
    def foreach(repr: CC[A])(f: A => Unit): Unit = repr foreach f
  }
  implicit def foreachIs[A] : ForeachIs[A]                                      = new ForeachIs[A]
  implicit def traversableIs[CC[X] <: Traversable[X], A] : TraversableIs[CC, A] = new TraversableIs[CC, A]
}
object DirectAccess {
  trait Impl[A0, Repr, CC0[X]] extends DirectAccess[Repr] {
    @inline final def foreach(repr: Repr)(f: A => Unit): Unit = length(repr) foreachIndex (i => f(elemAt(repr)(i)))
    type CC[X] = CC0[X]
    type A     = A0
  }
  final class ArrayIs[A: ClassTag] extends Impl[A, Array[A], Direct] {
    def length(repr: Array[A]): Size            = Size(repr.length)
    def elemAt(repr: Array[A])(index: Index): A = repr(index.value)
  }
  final class IndexedSeqIs[CC[X] <: IndexedSeq[X], A] extends Impl[A, CC[A], CC] {
    def length(repr: CC[A]): Size            = Size(repr.length)
    def elemAt(repr: CC[A])(index: Index): A = repr(index)
  }
  final class PspIndexedIs[A0] extends Impl[A0, Direct[A0], Direct] {
    def length(repr: Direct[A0]): Size             = repr.size
    def elemAt(repr: Direct[A0])(index: Index): A0 = repr elemAt index
  }

  implicit def pspIndexedIs[A] : PspIndexedIs[A]                                                          = new PspIndexedIs[A]
  implicit def arrayIs[A: ClassTag] : ArrayIs[A]                                                          = new ArrayIs[A]
  implicit def indexedSeqIs[CC[X] <: IndexedSeq[X], A](implicit z: Builds[A, CC[A]]): IndexedSeqIs[CC, A] = new IndexedSeqIs[CC, A]
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

object Does {
  trait DirectAccessImpl[A0, Repr, CC0[X]] extends DirectAccess[Repr] {
    @inline final def foreach(repr: Repr)(f: A => Unit): Unit = length(repr) foreachIndex (i => f(elemAt(repr)(i)))
    type CC[X] = CC0[X]
    type A     = A0
  }
  final class ArrayIsDirectAccess[A: ClassTag] extends DirectAccessImpl[A, Array[A], Direct] {
    def length(repr: Array[A]): Size            = Size(repr.length)
    def elemAt(repr: Array[A])(index: Index): A = repr(index.value)
  }
  final class IndexedSeqIsDirectAccess[CC[X] <: IndexedSeq[X], A] extends DirectAccessImpl[A, CC[A], CC] {
    def length(repr: CC[A]): Size            = Size(repr.length)
    def elemAt(repr: CC[A])(index: Index): A = repr(index)
  }
  final class PspIndexedIsDirectAccess[A0] extends DirectAccessImpl[A0, Direct[A0], Direct] {
    def length(repr: Direct[A0]): Size             = repr.size
    def elemAt(repr: Direct[A0])(index: Index): A0 = repr elemAt index
  }
}
