package psp
package std

import api._, StdEq._

final class ZipView[+A1, +A2](xs: View[A1], ys: View[A2]) {
  def zfoldl[B](f: (B, A1, A2) => B)(implicit z: Empty[B]): B = foldl(z.empty)(f)
  def foldl[B](zero: B)(f: (B, A1, A2) => B): B = {
    var res = zero
    foreach ((x, y) => res = f(res, x, y))
    res
  }
  def find(p: (A1, A2) => Boolean): Option[(A1, A2)] = {
    foreach((x, y) => if (p(x, y)) return Some(x -> y))
    None
  }
  def foreach(f: (A1, A2) => Unit): Unit = (xs, ys) match {

    case (xs: Direct[A1], ys: Direct[A2]) => (xs.size min ys.size).indices foreach (i => f(xs(i), ys(i)))
    case (xs: Direct[A1], ys)             => (ys take xs.size).foreachWithIndex((y, i) => f(xs(i), y))
    case (xs, ys: Direct[A2])             => (xs take ys.size).foreachWithIndex((x, i) => f(x, ys(i)))
    case _                                =>
      val it = xs.iterator
      ys foreach (y => if (it.hasNext) f(it.next, y) else return)
  }

  def flatMap[B](f: (A1, A2) => View[B]): View[B] = inView(mf => foreach((x, y) => f(x, y) foreach mf))
  def map[B](f: (A1, A2) => B): View[B]           = inView(mf => foreach((x, y) => mf(f(x, y))))
  def corresponds(f: (A1, A2) => Boolean)         = this map f forallTrue
  def drop(n: Precise): ZipView[A1, A2]           = new ZipView(xs drop n, ys drop n)
  def filter(q: Predicate2[A1, A2])               = withFilter(q)
  def pairs: View[(A1, A2)]                       = inView(mf => foreach((x, y) => mf(x -> y)))
  def take(n: Precise): ZipView[A1, A2]           = new ZipView(xs take n, ys take n)
  def toMap[A0 >: A1]: sciMap[A0, A2]             = this map (_ -> _) toScalaMap
  def withFilter(q: Predicate2[A1, A2])           = inView[(A1, A2)](mf => foreach((x, y) => if (q(x, y)) mf(x -> y)))

  def takeWhileLeft(q: Predicate[A1])  = pairs takeWhile (xy => q(fst(xy)))
  def takeWhileRight(q: Predicate[A2]) = pairs takeWhile (xy => q(snd(xy)))

  def filterLeft(q: Predicate[A1])  = withFilter((x, y) => q(x))
  def filterRight(q: Predicate[A2]) = withFilter((x, y) => q(y))

  def mapLeft[B1](g: A1 => B1)  = new ZipView(xs map g, ys)
  def mapRight[B2](g: A2 => B2) = new ZipView(xs, ys map g)

  def findLeft(p: Predicate[A1])  = find((x, y) => p(x))
  def findRight(p: Predicate[A2]) = find((x, y) => p(y))
}
