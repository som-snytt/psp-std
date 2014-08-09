package psp
package std

trait Implicits {
  // Reluctantly lifting Ints to these types else the syntactic pain is too high.
  implicit def liftIntToIndex(x: Int): Index   = Index(x)
  implicit def liftIntToOffset(x: Int): Offset = Offset(x)

  implicit def stringExtensionOps(s: String): StringExtensionOps                                       = new StringExtensionOps(s)
  implicit def seqExtensionOps[CC[X] <: scala.collection.Seq[X], A](xs: CC[A]): SeqExtensionOps[CC, A] = new SeqExtensionOps[CC, A](xs)
  implicit def arrayExtensionOps[A](xs: Array[A]): ArrayExtensionOps[A]                                = new ArrayExtensionOps[A](xs)
  implicit def anyExtensionOps[A](x: A): AnyExtensionOps[A]                                            = new AnyExtensionOps[A](x)
  implicit def tryExtensionOps[A](x: scala.util.Try[A]): TryExtensionOps[A]                            = new TryExtensionOps[A](x)
}

final class SeqExtensionOps[CC[X] <: scala.collection.Seq[X], A](/*private */ val xs: CC[A]) extends AnyVal {
  // An added benefit of these methods is a huge increase in type safety
  // because the sequence is treated as invariant due to an idiosyncrasy of
  // scala's type inference.
  def index(elem: A): Index                    = Index(xs indexOf elem)
  def lastIndex(elem: A): Index                = Index(xs lastIndexOf elem)
  def indexAtWhich(p: A => Boolean): Index     = Index(xs indexWhere p)
  def lastIndexAtWhich(p: A => Boolean): Index = Index(xs lastIndexWhere p)
  def indicesOf: IndexRange                    = IndexRange.until(0, xs.length)
  def hasElem(elem: A): Boolean                = xs contains elem

  // Produces a vector containing the elements in the given index range.
  // Ignores indices which don't exist in the target sequence.
  def apply(range: IndexRange): Vector[A] = indicesOf intersect range map (i => xs(i.value))
}

// Another demonstration of scala's boilerplate reduction powers.
final class StringExtensionOps(private val xs: String) extends AnyVal {
  def index(elem: Char): Index                    = Index(xs indexOf elem)
  def lastIndex(elem: Char): Index                = Index(xs lastIndexOf elem)
  def indexAtWhich(p: Char => Boolean): Index     = Index(xs indexWhere p)
  def lastIndexAtWhich(p: Char => Boolean): Index = Index(xs lastIndexWhere p)
  def indicesOf: IndexRange                       = IndexRange.until(0, xs.length)
  def hasElem(elem: Char): Boolean                = (xs indexOf elem) >= 0

  def apply(range: IndexRange): String = (indicesOf intersect range) |> (r => xs.slice(r.startInt, r.endInt))
  // indicesOf intersect range map (i => xs(i.value))
}

final class ArrayExtensionOps[A](private val xs: Array[A]) extends AnyVal {
  def index(elem: A): Index                    = Index(xs indexOf elem)
  def lastIndex(elem: A): Index                = Index(xs lastIndexOf elem)
  def indexAtWhich(p: A => Boolean): Index     = Index(xs indexWhere p)
  def lastIndexAtWhich(p: A => Boolean): Index = Index(xs lastIndexWhere p)
  def indicesOf: IndexRange                    = IndexRange.until(0, xs.length)
  def hasElem(elem: A): Boolean                = xs contains elem

  def apply(range: IndexRange)(implicit tag: reflect.ClassTag[A]): Array[A] = (indicesOf intersect range) |> (r => xs.slice(r.startInt, r.endInt))
  // indicesOf intersect range map (i => xs(i.value))
}

final class AnyExtensionOps[A](val x: A) extends AnyVal {
  // The famed forward pipe.
  @inline def |>[B](f: A => B): B = f(x)
}

final class TryExtensionOps[A](val x: scala.util.Try[A]) extends AnyVal {
  def fold[B](f: A => B, g: Throwable => B): B = x match {
    case scala.util.Success(x) => f(x)
    case scala.util.Failure(t) => g(t)
  }
}
