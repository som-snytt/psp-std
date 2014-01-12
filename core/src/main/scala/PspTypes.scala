package psp
package core

/** A thin abstraction over some questionable assumptions. */
trait PspTypes {
  type Index         = Int
  type Suspended[+A] = (A => Unit) => Unit
}

trait LowPriorityPsp {
  // This implicit is for "Reprs" like String and Array
  implicit def mkIndirectViewOps[Coll](xs: Coll)(implicit itl: IsTraversableLike[Coll]): View[Coll, itl.A] =
    new View[Coll, itl.A](xs)(xs => (itl conversion xs).seq.toTraversable)
}

trait MidPriorityPsp extends LowPriorityPsp {
  implicit def mkPartiallyOrderOps[A](x: PartiallyOrdered[A]): PartiallyOrderedOps[A]                  = new PartiallyOrderedOps(x)
  // This implicit is for collections like Seq[A]
  implicit def mkDirectViewOps[Coll, A](xs: Coll)(implicit ev: Coll <:< Traversable[A]): View[Coll, A] = new View[Coll, A](xs)(ev)
}
