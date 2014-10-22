package psp
package std

import api._

trait StdZipped {
  // There's a great deal of pain associated with trying to make this appear as an instance method.
  // Requiring a call to this method means scala can figure shit out even on 2.10. It's an easy decision.
  // def zip2[A1, R1, A2, R2](xs: R1, ys: R2)(implicit tc1: Walks[A1, R1], tc2: Walks[A2, R2]): Zip2Ops[A1, R1, A2, R2]                                             = new Zip2Ops(xs, ys, (x, y) => true)

  def zipIndex[A1, R1](xs: R1)(implicit tc1: Walks[A1, R1]): Zip2Ops[A1, Index] = zip2(xs, Foreach from 0 map (i => Index(i)))

  def zip2[A1, R1, A2, R2](xs: R1, ys: R2)(implicit tc1: Walks[A1, R1], tc2: Walks[A2, R2]): Zip2Ops[A1, A2] = new Zip2Ops(tc1 wrap xs, tc2 wrap ys, (x, y) => true)

  def zip3[A1, R1, A2, R2, A3, R3](xs: R1, ys: R2, zs: R3)(implicit tc1: Walks[A1, R1], tc2: Walks[A2, R2], tc3: Walks[A3, R3]): Zip3Ops[A1, R1, A2, R2, A3, R3] = new Zip3Ops(xs, ys, zs)

  class MappedZip2Ops[A1, A2, B1, B2](zops: Zip2Ops[A1, A2], f: A1 => B1, g: A2 => B2) {
    def toMap(implicit z: HashEq[B1]): pMap[B1, B2] = zops.map[(B1, B2), sciMap[B1, B2]]((k, v) => f(k) -> g(v)).toPolicyMap
  }

  // final class Zip2Ops[A1, R1, A2, R2](xs: R1, ys: R2, p: Predicate2[A1, A2])(implicit tc1: Walks[A1, R1], tc2: Walks[A2, R2]) {

  final class Zip2Ops[A1, A2](xs: Foreach[A1], ys: Foreach[A2], p: Predicate2[A1, A2]) {
    type This = Zip2Ops[A1, A2]
    def filterLeft(q: Predicate[A1]): This      = withFilter((x, y) => q(x))
    def filterRight(q: Predicate[A2]): This     = withFilter((x, y) => q(y))
    def filter(q: Predicate2[A1, A2]): This     = withFilter(q)
    def withFilter(q: Predicate2[A1, A2]): This = new Zip2Ops(xs, ys, (x, y) => p(x, y) && q(x, y))

    def find(p: (A1, A2) => Boolean): Option[(A1, A2)] = {
      foreach((x, y) => if (p(x, y)) return Some(x -> y))
      None
    }
    def mapLeft[B1](g: A1 => B1)  = new MappedZip2Ops(this, g, identity[A2])
    def mapRight[B2](g: A2 => B2) = new MappedZip2Ops(this, identity[A1], g)

    def map[C, That](g: (A1, A2) => C)(implicit z: Builds[C, That]): That = z direct (f => foreach((x, y) => f(g(x, y))))
    def toMap: sciMap[A1, A2]                                             = map[(A1, A2), sciMap[A1, A2]](_ -> _)

    def corresponds(f: (A1, A2) => Boolean): Boolean = map(f) forall (_ == true)

    def foreach(f: (A1, A2) => Unit): Unit = xs.m.biIterator doto (it =>
      ys.m foreach (y =>
        if (it.hasNext)
          it.next |> (x => if (p(x, y)) f(x, y))
        else
          return
      )
    )
  }

  final class Zip3Ops[A1, R1, A2, R2, A3, R3](xs: R1, ys: R2, zs: R3)(implicit tc1: Walks[A1, R1], tc2: Walks[A2, R2], tc3: Walks[A3, R3]) {
    def map[C, That](g: (A1, A2, A3) => C)(implicit z: Builds[C, That]): That = z direct (f =>
      xs.m.biIterator doto (itX =>
        ys.m.biIterator doto (itY =>
          zs.m foreach (z =>
            if (itX.hasNext && itY.hasNext)
              f(g(itX.next, itY.next, z))
          )
        )
      )
    )
  }
  // Implicit approach - works on 2.11, not on 2.10, ugly as hell in all lands.
  //
  // implicit class Tuple2WalkableOps[R1, A1, R2, A2](val xy: (R1, R2))(implicit tc1: Walks[A1, R1], tc2: Walks[A2, R2]) {
  //   def map[C, That](g: (A1, A2) => C)(implicit z: Builds[C, That]): That =
  //     z(f => xy._1.m.biIterator doto (it => xy._2.m foreach (y => f(g(it.next, y)))))
  // }
  // implicit class Tuple3WalkableOps[R1, A1, R2, A2, R3, A3](val xy: (R1, R2, R3))(implicit tc1: Walks[A1, R1], tc2: Walks[A2, R2], tc3: Walks[A3, R3]) {
  //   def map[C, That](g: (A1, A2, A3) => C)(implicit z: Builds[C, That]): That = z { f =>
  //     val it1 = (tc1 wrap xy._1).biIterator
  //     val it2 = (tc2 wrap xy._2).biIterator
  //     (tc3 foreach xy._3)(z => if (it1.hasNext && it2.hasNext) f(g(it1.next, it2.next, z)))
  //   }
  // }
}
