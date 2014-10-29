package psp
package std

import api._, StdEq._

trait StdZipped {
  def zipIndex[A](xs: Each[A]): Zip2Ops[A, Index]            = zip2(xs, Each from 0 map (i => Index(i)))
  def zip2[A, B](xs: Each[A], ys: Each[B]): Zip2Ops[A, B] = new Zip2Ops(xs, ys, (x, y) => true)

  class MappedZip2Ops[A1, A2, B1, B2](zops: Zip2Ops[A1, A2], f: A1 => B1, g: A2 => B2) {
    def toMap(implicit z: HashEq[B1]): exMap[B1, B2] = zops map ((k, v) => f(k) -> g(v)) pmap
  }

  final class Zip2Ops[A1, A2](xs: Each[A1], ys: Each[A2], p: Predicate2[A1, A2]) {
    type This = Zip2Ops[A1, A2]

    def corresponds(f: (A1, A2) => Boolean): Boolean = map(f) forall (_ == true)
    def filter(q: Predicate2[A1, A2]): This          = withFilter(q)
    def filterLeft(q: Predicate[A1]): This           = withFilter((x, y) => q(x))
    def filterRight(q: Predicate[A2]): This          = withFilter((x, y) => q(y))
    def find(p: (A1, A2) => Boolean)                 = pairs find p.tupled
    def mapLeft[B1](g: A1 => B1)                     = new MappedZip2Ops(this, g, identity[A2])
    def mapRight[B2](g: A2 => B2)                    = new MappedZip2Ops(this, identity[A1], g)
    def map[C](g: (A1, A2) => C): View[C]            = inView(f => foreach((x, y) => f(g(x, y))))
    def pairs: View[(A1, A2)]                        = inView(mf => foreach((x, y) => mf(x -> y)))
    def toMap: sciMap[A1, A2]                        = map(_ -> _).toScalaMap
    def withFilter(q: Predicate2[A1, A2]): This      = new Zip2Ops(xs, ys, (x, y) => p(x, y) && q(x, y))

    def foreach(f: (A1, A2) => Unit): Unit = xs.iterator doto (it =>
      ys.m foreach (y =>
        if (it.hasNext)
          it.next |> (x => if (p(x, y)) f(x, y))
        else
          return
      )
    )
  }
}
