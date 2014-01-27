package psp
package core

/** A thin abstraction over some questionable assumptions. */
trait PspTypes {
  type Index         = Int
  type Done          = Boolean
  type Suspended[+A] = (A => Unit) => Unit
  type Ref[+A]       = A with AnyRef
  val MaxIndex       = Int.MaxValue
  val NoIndex        = -1
  val EOL            = sys.props.getOrElse("line.separator", "\n")

  type ForeachableType[Repr, CC0[X], A0] = Foreachable[Repr] {
    type A = A0
    type CC[B] = CC0[B]
  }
  type IndexableType[Repr, CC0[X], A0] = Indexable[Repr] {
    type A = A0
    type CC[B] = CC0[B]
  }
  type ForeachableSetType[Repr, CC0[X], A0] = ForeachableSet[Repr] {
    type A = A0
    type CC[B] = CC0[B]
  }
}
