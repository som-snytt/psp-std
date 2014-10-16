package psp
package std

import api._

object Generator {
  type Gen[+A]  = Generator[A]
  type Susp[+A] = SuspendedTo[A, Generator]
  object EOS extends RuntimeException("end of stream") with ControlThrowable

  val Empty: Gen[Nothing] = create[Nothing](_ => throw EOS)

  def apply[A](xs: scTraversableOnce[A]): Gen[A] = apply[A](xs.toIterator)
  def apply[A](it: sIterator[A]): Gen[A]         = new IteratorGenerator[A](it)
  def apply[A](xs: sciLinearSeq[A]): Gen[A]      = new LinearGenerator[A](xs)
  def apply[A](xs: sciStream[A]): Gen[A]         = new StreamGenerator[A](xs)
  def from(n: Int): IntGenerator                 = new IntGenerator(n)
  def from(n: Long): LongGenerator               = new LongGenerator(n)

  def unfold[A](zero: A)(f: A => A): Gen[A] = new UnfoldGenerator(zero, f)

  @tailrec private def filterer[A](xs: Gen[A], p: A => Boolean, f: A => Unit): Gen[A] = {
    if (xs.isEmpty) Empty else {
      var done = false
      val next = xs(x => if (p(x)) try f(x) finally done = true)
      if (done) next filter p else filterer(next, p, f)
    }
  }
  @tailrec private def dropper[A](xs: Gen[A], n: Int, f: A => Unit): Gen[A] = (
    if (xs.isEmpty) Empty
    else if (n <= 0) xs(f)
    else dropper(xs.tail, n - 1, f)
  )

  def empty[A] : Gen[A]                                = Empty
  def create[A](mf: Susp[A]): Gen[A]                   = new Impl(mf)
  def concat[A](xs: Gen[A], ys: Gen[A]): Gen[A]        = create[A](f => Try(concat(xs(f), ys)) || ys(f) | empty[A])
  def mapped[A, B](xs: Gen[A], f: A => B): Gen[B]      = create[B](g => xs(x => g(f(x))) map f)
  def flatten[A](xss: Gen[Gen[A]]): Gen[A]             = xss.fold(empty[A])(_ ++ _)
  def taken[A](xs: Gen[A], n: Int): Gen[A]             = create[A](f => if (xs.isEmpty || n <= 0) Empty else taken(xs(f), n - 1))
  def dropped[A](xs: Gen[A], n: Int): Gen[A]           = create[A](dropper(xs, n, _))
  def filtered[A](xs: Gen[A], p: A => Boolean): Gen[A] = create[A](filterer(xs, p, _))

  class Impl[A](val mf: Susp[A]) extends AnyVal with Gen[A] {
    def apply(f: A => Unit): Gen[A] = Try(mf(f)) | empty[A]
  }
  final class IntGenerator(val value: Int) extends AnyVal with Gen[Int] {
    def apply(f: Int => Unit): IntGenerator = new IntGenerator(value + 1) sideEffect f(value)
  }
  final class LongGenerator(val value: Long) extends AnyVal with Gen[Long] {
    def apply(f: Long => Unit): LongGenerator = new LongGenerator(value + 1) sideEffect f(value)
  }
  final class UnfoldGenerator[A](elem: A, next: A => A) extends Gen[A] {
    def apply(f: A => Unit): Gen[A] = try new UnfoldGenerator[A](next(elem), next) finally f(elem)
  }
  final case class Zipped[A, B](lhs: Gen[A], rhs: Gen[B]) extends Gen[(A, B)] {
    def apply(f: ((A, B)) => Unit): Gen[(A, B)] = {
      if (lhs.isEmpty || rhs.isEmpty) Empty else {
        var a = nullAs[A]
        var b = nullAs[B]
        try lhs(a = _) zip rhs(b = _) finally f(a -> b)
      }
    }
  }
  final case class Interspersed[A](lhs: Gen[A], rhs: Gen[A]) extends Gen[A] {
    def apply(f: A => Unit): Gen[A] = Try(if (lhs.isEmpty) rhs(f) else new Interspersed(rhs, lhs(f))) | empty[A]
  }
  final case class Cyclic[A](cycle: Gen[A], xs: Gen[A]) extends Gen[A] {
    def apply(f: A => Unit): Gen[A] = (
      if (xs.isEmpty) new Cyclic(cycle, cycle(f))
      else try new Cyclic(cycle, xs(f))
      catch { case EOS => new Cyclic(cycle, cycle(f)) }
    )
  }

  final class IteratorGenerator[A](private val it: sIterator[A]) extends Gen[A] {
    def memo: Gen[A] = Generator[A](it.toStream)
    def apply(f: A => Unit): Gen[A] = try this finally f(it.next)
  }
  final class LinearGenerator[A](val xs: sciLinearSeq[A]) extends AnyVal with Gen[A] {
    def apply(f: A => Unit): Gen[A] = if (xs.isEmpty) Empty else try Generator(xs.tail) finally f(xs.head)
  }
  final class StreamGenerator[A](val xs: sciStream[A]) extends AnyVal with Gen[A] {
    def apply(f: A => Unit): Gen[A] = if (xs.isEmpty) Empty else try Generator(xs.tail) finally f(xs.head)
  }
}

package ops {
  import Generator._

  final class GeneratorOps[A](val g: Generator.Gen[A]) extends AnyVal {
    def nonEmpty          = !isEmpty
    def isEmpty           = g id_== Empty
    def size: PreciseSize = fold(0.size)((res, _) => res + 1.size)
    def tail: Gen[A]      = if (g.isEmpty) Empty else g(_ => ())

    def take(n: Int): Gen[A]               = taken(g, n)
    def drop(n: Int): Gen[A]               = dropped(g, n)
    def zip[B](h: Gen[B]): Gen[(A, B)]     = Zipped(g, h)
    def map[B](f: A => B): Gen[B]          = mapped[A, B](g, f)
    def flatMap[B](f: A => Gen[B]): Gen[B] = flatten(g map f)

    def memo: Gen[A] = g match {
      case x: IteratorGenerator[_] => x.memo
      case _                       => g
    }
    def ++[A1 >: A](h: Gen[A1]): Gen[A1]          = concat[A1](g, h)
    def intersperse[A1 >: A](h: Gen[A1]): Gen[A1] = Interspersed(g, h)
    def cyclic: Gen[A]                            = memo |> (c => Cyclic(c, c))

    // @tailrec
    def foreach(f: A => Unit): Unit = if (nonEmpty) g(f) foreach f

    @inline def reduce(f: (A, A) => A): A = {
      var nonEmpty = false
      var first = nullAs[A]
      val result = g(x => try first = x finally nonEmpty = true).fold(first)(f)
      if (nonEmpty) result else abort("empty.reduce")
    }
    @inline def fold[B](zero: B)(f: (B, A) => B): B = {
      def loop(gen: Gen[A], in: B): B = {
        var out = in
        val next = gen(x => out = f(out, x))
        if (next.isEmpty) out else loop(next, out)
      }
      loop(g, zero)
    }
    @inline def withFilter(p: A => Boolean): Gen[A] = filtered(g, p)
    @inline def filter(p: A => Boolean): Gen[A]     = filtered(g, p)
  }
}
