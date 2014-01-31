package psp
package core
package api

trait ViewEnvironment[Coll, CC[X], A] {
  def unknownView(tc: ForeachableType[Coll, CC, A]): AtomicView
  def linearView(tc: LinearableType[Coll, CC, A]): LinearView
  def indexedView(tc: IndexableType[Coll, CC, A]): IndexedView

  type View[+X] <: api.View[X]
  type AtomicView <: View[A] with Foreach[A]
  type LinearView <: AtomicView with Linear[A]
  type IndexedView <: AtomicView with Indexed[A]
}
