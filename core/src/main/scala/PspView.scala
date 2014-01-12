package psp
package core

sealed trait PspView[+A] extends Foreach[A] with Viewable[A] {
  type CC[+X] = PspView[X]

  def underlying: Foreach[A]

  protected[this] def unlessEmpty[B](nextSize: SizeInfo)(g: => Foreach[B]): PspView[B] =
    if (nextSize.isZero) PspView() else PspView[B](g, nextSize)

  def flatMap[B](f: A => Foreach[B]): CC[B] = PspView(Foreach.flatMapped(underlying)(f))
  def collect[B](pf: A =?> B): CC[B]        = PspView(Foreach.collected(underlying)(pf))
  def filter(p: A => Boolean): Self         = PspView(Foreach.filtered(underlying)(p))
  def filterNot(p: A => Boolean): Self      = PspView(Foreach.filtered[A](underlying)(x => !p(x)))
  def takeWhile(p: A => Boolean): Self      = PspView(Foreach[A](g => applyWhile(p)(g)), sizeInfo.atMost)
  def dropWhile(p: A => Boolean): Self      = PspView(Foreach[A](g => skipWhile(p)(g)), sizeInfo.atMost)
  def head: A                               = { underlying foreach (x => return x) ; failEmpty("head") }
  def mkString(sep: String)                 = underlying mk_s sep

  @inline final def foreach(f: A => Unit): Unit = underlying foreach f
  @inline final def foreachWithCount(f: (A, Index) => Boolean): Unit = {
    var count = 1
    foreach (x => if (f(x, count)) count += 1 else return )
  }

  protected[this] def andTrue(x: Unit): Boolean = true

  // Caution required not to traverse one more than intended.
  protected[this] def applyN(n: Int)(f: A => Unit): Unit              = if (n > 0) foreachWithCount((x, count) => andTrue(f(x)) && count < n)
  protected[this] def skipN(n: Int)(f: A => Unit): Unit               = foreachWithCount((x, count) => (count <= n) || andTrue(f(x)))
  protected[this] def sliceN(s: Int, e: Int)(f: A => Unit): Unit      = foreachWithCount((x, count) => (count <= e) && { if (s < count) f(x) ; true })
  protected[this] def applyWhile(p: A => Boolean)(f: A => Unit): Unit = foreach (x => if (p(x)) f(x) else return)

  protected[this] def skipWhile(p: A => Boolean)(f: A => Unit): Unit  = {
    var done = false
    foreach { x =>
      if (!done && !p(x)) done = true
      if (done) f(x)
    }
  }

  override def toString = ss"$underlying"
}

object PspView {
  lazy val Empty: Sliceable[Nothing] = Sliceable(Indexed.Empty, Size(0))

  final case class Unsliceable[+A](underlying: Foreach[A], sizeInfo: SizeInfo) extends PspView[A] {
    import Foreach._

    private def exhaust[A](capacity: Int, xs: Foreach[A]): CircularBuffer[A] = CircularBuffer[A](Size(capacity)) ++= xs
    private def takeImpl(n: Int): Foreach[A]      = if (n <= 0) empty else Foreach(g => applyN(n)(g))
    private def takeRightImpl(n: Int): Foreach[A] = if (n <= 0) empty else exhaust(n, underlying)
    private def dropImpl(n: Int): Foreach[A]      = if (n <= 0) underlying else Foreach(g => skipN(n)(g))
    private def dropRightImpl(n: Int): Foreach[A] = if (n <= 0) underlying else (
      Foreach[A](g => underlying.foldl(CircularBuffer[A](Size(n)))((buf, x) => if (buf.isFull) try buf finally g(buf push x) else buf += x))
    )

    def map[B](f: A => B): CC[B]              = Unsliceable(new LinearMapped(underlying, f), sizeInfo)
    def take(n: Int): Self                    = unlessEmpty(sizeInfo min precise(n))(takeImpl(n) labeled ss"$underlying take $n")
    def takeRight(n: Int): Self               = unlessEmpty(sizeInfo min precise(n))(takeRightImpl(n) labeled ss"$underlying takeRight $n")
    def drop(n: Int): Self                    = unlessEmpty(sizeInfo - precise(n))(dropImpl(n) labeled ss"$underlying drop $n")
    def dropRight(n: Int): Self               = unlessEmpty(sizeInfo - precise(n))(dropRightImpl(n) labeled ss"$underlying dropRight $n")
    def slice(start: Int, end: Int): Self     = unlessEmpty(sizeInfo min precise(end - start))(Foreach[A](g => sliceN(start, end)(g)) labeled ss"$underlying.slice($start, $end)")

    def tail: Self = unlessEmpty(sizeInfo - precise(1))(Foreach[A](g => skipN(1)(g)))
  }

  final case class Sliceable[+A](underlying: Indexed[A], size: Size) extends PspView[A] {
    def sizeInfo = SizeInfo.Precise(size)
    private def intSize = size.value
    import Indexed.Sliced

    private[this] def sliced(start: Int, end: Int): Self = {
      val nextSize = Size(end - start)
      val slice = underlying match {
        case Sliced(xs, range) => new Sliced(xs, range drop start take nextSize.value)
        case _                 => new Sliced(underlying, Interval(start, end))
      }
      Sliceable(slice, nextSize)
    }

    def map[B](f: A => B): CC[B]    = Sliceable(new IndexedMapped(underlying, f), size)
    def take(n: Int): Self          = if (n <= 0) Empty else if (intSize <= n) this else sliced(0, n)
    def takeRight(n: Int): Self     = if (n <= 0) Empty else if (intSize <= n) this else sliced(intSize - n, intSize)
    def drop(n: Int): Self          = if (n <= 0) this else if (intSize <= n) Empty else sliced(n, intSize)
    def dropRight(n: Int): Self     = if (n <= 0) this else if (intSize <= n) Empty else sliced(0, intSize - n)
    def slice(s: Int, e: Int): Self = if (e <= s || e <= 0) Empty else sliced(s max 0, e min intSize)
    def tail: Self                  = if (size.isZero) failEmpty("tail") else drop(1)
  }

  def apply[A](): PspView[A] = Empty
  def apply[A](xs: Foreach[A]): PspView[A] = apply(xs, SizeInfo(xs))
  def apply[A](xs: Foreach[A], sizeInfo: SizeInfo): PspView[A] = (xs, sizeInfo) match {
    case (xs: Indexed[_], SizeInfo.Precise(size)) => new Sliceable(xs, size)
    case _                                        => new Unsliceable(xs, sizeInfo)
  }
}
