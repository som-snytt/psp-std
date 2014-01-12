package psp
package core
package linear

trait PspLinear[+A] extends Any with Foreach[A] {
  def isEmpty: Boolean
  def head: A
  def tail: PspLinear[A]
  def foreach(f: A => Unit): Unit
}

object PspList {
  // implicit def nilIsCovariant[A](xs: nil.type): PspList[A] = empty[A]

  def empty[A] = nil.castTo[PspList[A]]
  def fill[A](n: Int)(body: => A): PspList[A] = apply(0 until n map (_ => body): _*)
  def apply[A](xs: A*): PspList[A]            = xs.foldRight(nil[A]())(_ :: _)

  def stringOf[A](xs: PspList[A], max: Int = 7): String = {
    def loop(n: Int, xs: PspList[A], res: String): String = (
      if (n <= 0 || xs.isEmpty) res + ( if (xs.isEmpty) " :: nil" else " :: ..." )
      else if (res == "") loop(n - 1, xs.tail, xs.head.to_s)
      else loop(n - 1, xs.tail, res + " :: " + xs.head.to_s)
    )
    loop(max, xs, "")
  }
}

sealed trait PspList[A] extends AnyRef with PspLinear[A] {
  def tail: PspList[A]
  def sizeInfo = if (isEmpty) precise(0) else precise(1).atLeast
  private def upcast[A1 >: A] : PspList[A1] = this.castTo[PspList[A1]]

  def reverse: PspList[A] = {
    def loop(in: PspList[A], out: PspList[A]): PspList[A] = if (in.isEmpty) out else loop(in.tail, in.head :: out)
    loop(this, PspList.empty[A])
  }

  def take(n: Int): PspList[A] = {
    @tailrec def loop(n: Int, in: PspList[A], out: PspList[A]): PspList[A] =
      if (n <= 0 || in.isEmpty) out.reverse else loop(n - 1, in.tail, in.head :: out)

    loop(n, this, PspList[A]())
  }
  def drop(n: Int): PspList[A] = {
    @tailrec def loop(n: Int, xs: PspList[A]): PspList[A] = if (n <= 0 || xs.isEmpty) xs else loop(n - 1, xs.tail)
    loop(n, this)
  }

  def ::(x: A): PspList[A] = new ::(x, this)
  def :::(xs: PspList[A]): PspList[A] = xs match {
    case x :: xs => x :: (xs ::: this)
    case _       => this
  }

  def contains(x: A): Boolean = {
    @tailrec def loop(xs: PspList[A]): Boolean = xs match {
      case y :: ys => (x == y) || loop(ys)
      case _       => false
    }
    loop(this)
  }
  @inline final def foreach(f: A => Unit): Unit = {
    @tailrec def loop(x: PspList[A]): Unit = x match {
      case x :: xs => f(x) ; loop(xs)
      case _       =>
    }
    loop(this)
  }

  final override def toString = if (isEmpty) "nil" else PspList.stringOf(this)
}

final case object nil extends PspList[Nothing] {
  def isEmpty  = true
  def head     = failEmpty("head")
  def tail     = failEmpty("tail")

  def apply[A](): PspList[A] = this.castTo[PspList[A]]
  def unapply[A](xs: PspList[A]): Boolean = this eq xs
}
final case class ::[A](head: A, tail: PspList[A]) extends PspList[A] {
  def isEmpty = false
}
