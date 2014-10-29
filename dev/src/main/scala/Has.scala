package psp
package std

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
  trait Size[-R]     extends Any with ElemType    { def hasSize(xs: R): api.Size                            }
  trait Contains[-R] extends Any with ElemType    { def hasContains(xs: R)(x: Elem): Boolean                }
  trait Each[-R]  extends Any with ElemType    { def hasForeach(xs: R)(f: Elem => Unit): Unit            }
  trait Filter[R]    extends Any with ElemType    { def hasFilter(xs: R)(p: Predicate[Elem]): R             }
  trait Index[-R]    extends Any with ElemType    { def hasIndex(xs: R)(index: api.Index): Elem             }
  trait View[R]      extends Any with ElemType    { def hasView(xs: R): View[Elem]                          }
  trait Apply[-R]    extends Any with ApplyType   { def hasApply(xs: R)(in: In): Out                        }
  trait Map[R]       extends Any with Is.Packaged { def hasMap[A](xs: R)(f: Elem => A): CC[A]               }
  trait FlatMap[R]   extends Any with Is.Packaged { def hasFlatMap[A](xss: R)(f: Elem => Each[A]): CC[A] }
}

object Is {
  trait Packaged extends Any with Has.CcType with Has.ElemType

  trait Container[M[X], A]       extends Any with Has.CcParam[M]     with Has.ElemParam[A]
  trait IndexContainer[M[X], A]  extends Any with Is.Container[M, A] with Is.Index[M[A]]
  trait LinearContainer[M[X], A] extends Any with Is.Container[M, A] with Is.Linear[M[A]]
  trait ForeachView[M[X], A]     extends Any with Has.Each[M[A]]  with Has.View[M[A]] with Has.Size[M[A]]
  trait Iterable[-R]             extends Any with Has.Each[R]     with Has.Size[R]
  trait Seq[-R]                  extends Any with Has.Each[R]
  trait Set[-R]                  extends Any with Has.Each[R]     with Has.Contains[R]
  trait Map[-R]                  extends Any with Has.Each[R]     with Has.Contains[R]  with Has.Apply[R]  { type Elem = In }
  trait Index[R]                 extends Any with Is.Seq[R]          with Has.Index[R]     with Has.Size[R]
  trait Linear[R]                extends Any with Is.Seq[R]          with Has.Head[R]      with Has.Tail[R] with Has.IsEmpty[R]

  abstract class Array[A] extends IndexContainer[Array, A]
}
