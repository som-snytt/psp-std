package psp
package core

sealed trait PspView[+A] extends Foreach[A] with Viewable[A] {
  type CC[+X] = PspView[X]

  def underlying: Foreach[A]

  import Foreach._

  def flatMap[B](f: A => Foreach[B]): CC[B]  = flatMapped(underlying)(f).view
  def collect[B](pf: A =?> B): CC[B]         = collected(underlying)(pf).view
  def ++[A1 >: A](that: Foreach[A1]): CC[A1] = joined(underlying, that).view
  def filter(p: A => Boolean): Self          = filtered(underlying)(p).view
  def filterNot(p: A => Boolean): Self       = filtered[A](underlying)(x => !p(x)).view
  def takeWhile(p: A => Boolean): Self       = Foreach[A](g => applyWhile(p)(g)) sized sizeInfo.atMost view
  def dropWhile(p: A => Boolean): Self       = Foreach[A](g => skipWhile(p)(g)) sized sizeInfo.atMost view
  def head: A                                = { underlying foreach (x => return x) ; failEmpty("head") }
  def mkString(sep: String)                  = underlying mk_s sep

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
  def someViewOps[T] = List[PspView[T] => PspView[T]](
    _ take 100,
    _ map identity,
    _ dropRight 30,
    _ dropRight 10,
    _ drop 10,
    _ filter (_ => true),
    _ flatMap (x => x -> x),
    _ take 20
  )
    // x => x ++ (nullAs[T] nTimes 10)
  def applySomeViewOps[T](zero: PspView[T]) = someViewOps[T].scanLeft("" -> zero) { case ((prev, res), f) =>
    val op = f(res)
    val str = (op.to_s stripPrefix res.to_s).trim
    str -> op
  }
  def showViewOps[T](zero: PspView[T]) = {
    for ((str, op) <- applySomeViewOps[T](zero)) {
      println("%15s  %-20s  %s".format(op.sizeInfo, str, (op take 3 mk_s ", ") + " ..."))
    }
  }

  lazy val Empty: Sliceable[Nothing] = Sliceable(Indexed.Empty, Size(0))
  def emptyView[A](label: String): PspView[A] = PspView(Empty labeled label)

  final case class Unsliceable[+A](underlying: Foreach[A], sizeInfo: SizeInfo) extends PspView[A] {
    private def xs = underlying

    protected[this] def unlessEmpty[B](nextSize: SizeInfo, label: String)(g: => Foreach[B]): PspView[B] =
      if (nextSize.isZero) emptyView[B](ss"$xs") else (g labeled ss"$underlying$label" sized nextSize).view

    private def exhaust[A](capacity: Int, xs: Foreach[A]): CircularBuffer[A] = CircularBuffer[A](Size(capacity)) ++= xs
    private def takeImpl(n: Int): Foreach[A]      = if (n <= 0) Foreach.empty else Foreach(g => applyN(n)(g))
    private def takeRightImpl(n: Int): Foreach[A] = if (n <= 0) Foreach.empty else exhaust(n, underlying)
    private def dropImpl(n: Int): Foreach[A]      = if (n <= 0) underlying else Foreach(g => skipN(n)(g))
    private def dropRightImpl(n: Int): Foreach[A] = if (n <= 0) underlying else (
      Foreach[A](g => underlying.foldl(CircularBuffer[A](Size(n)))((buf, x) => if (buf.isFull) try buf finally g(buf push x) else buf += x))
    )

    def map[B](f: A => B): CC[B]          = unlessEmpty(sizeInfo, ss" map $f")(Mapped(underlying, f))
    def take(n: Int): Self                = unlessEmpty(sizeInfo min precise(n), ss" take $n")(takeImpl(n))
    def takeRight(n: Int): Self           = unlessEmpty(sizeInfo min precise(n), ss" takeRight $n")(takeRightImpl(n))
    def drop(n: Int): Self                = unlessEmpty(sizeInfo - precise(n), ss" drop $n")(dropImpl(n))
    def dropRight(n: Int): Self           = unlessEmpty(sizeInfo - precise(n), ss" dropRight $n")(dropRightImpl(n))
    def slice(start: Int, end: Int): Self = unlessEmpty(sizeInfo min precise(end - start), ss".slice($start, $end)")(Foreach[A](g => sliceN(start, end)(g)))

    def tail: Self = unlessEmpty(sizeInfo - precise(1), ".tail")(Foreach[A](g => skipN(1)(g)))
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

    def map[B](f: A => B): CC[B]    = Sliceable(WithIndex(Mapped(underlying, f), idx => f(underlying(idx))), size)
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
