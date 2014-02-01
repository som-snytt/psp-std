package psp
package core
package api

trait ViewEnvironment[Repr, CC[X], A] {
  def unknownView(tc: ForeachableType[Repr, CC, A]): AtomicView
  def linearView(tc: LinearableType[Repr, CC, A]): LinearView
  def indexedView(tc: IndexableType[Repr, CC, A]): IndexedView

  type View[+X] <: api.View[X]
  type AtomicView <: View[A] with Foreach[A]
  type LinearView <: AtomicView with Linear[A]
  type IndexedView <: AtomicView with Indexed[A]
}
