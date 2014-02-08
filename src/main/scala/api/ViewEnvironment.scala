package psp
package core
package api

trait ViewEnvironment[A, Repr, CC[X]] {
  def unknownView(tc: ForeachableType[A, Repr, CC]): AtomicView
  def linearView(tc: SequentialAccessType[A, Repr, CC]): LinearView
  def indexedView(tc: DirectAccessType[A, Repr, CC]): IndexedView

  type View[+X] <: api.BuilderView[X, Repr]
  type AtomicView <: View[A]
  type LinearView <: AtomicView with Linear[A]
  type IndexedView <: AtomicView with Direct[A]
}
