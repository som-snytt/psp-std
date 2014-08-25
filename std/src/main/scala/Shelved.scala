
// def zip[B](that: Indexed[B]): Indexed[(A, B)]                                                   = zipWith(that)(_ -> _)
// def zipWith[A1, B](that: Indexed[A1])(f: (A, A1) => B): Indexed[B]                              = new ZippedIndexed2(xs, that, f)
// def zipWith[A1, A2, B](that1: Indexed[A1], that2: Indexed[A2])(f: (A, A1, A2) => B): Indexed[B] = new ZippedIndexed3(xs, that1, that2, f)
// final class ZippedIndexed2[A, B, +C](left: Indexed[A], right: Indexed[B], f: (A, B) => C) extends Indexed[C] {
//   def foreach(f: C => Unit): Unit = {
//     var i = zero
//     while (isDefinedAt(i)) { f(elemAt(i)); i = i.next }
//   }
//   def isDefinedAt(index: Index): Boolean = (left isDefinedAt index) && (right isDefinedAt index)
//   def apply(index: Index): C             = elemAt(index)
//   def elemAt(index: Index): C            = f(left elemAt index, right elemAt index)
//   def sizeInfo: SizeInfo               = left.sizeInfo min right.sizeInfo
// }
// final class ZippedIndexed3[A, A1, A2, +B](xs1: Indexed[A], xs2: Indexed[A1], xs3: Indexed[A2], f: (A, A1, A2) => B) extends Indexed[B] {
//   def foreach(f: B => Unit): Unit = {
//     var i = zero
//     while (isDefinedAt(i)) { f(elemAt(i)); i = i.next }
//   }
//   def isDefinedAt(index: Index): Boolean = (xs1 isDefinedAt index) && (xs2 isDefinedAt index) && (xs3 isDefinedAt index)
//   def apply(index: Index): B             = elemAt(index)
//   def elemAt(index: Index): B            = f(xs1(index), xs2(index), xs3(index))
//   def sizeInfo: SizeInfo                 = xs1.sizeInfo min xs2.sizeInfo min xs3.sizeInfo
// }

// implicit object IsString extends Is.Index[String] with Has.Contains[String] with Has.Map[String] with Has.Filter[String] {
//   type CC[X]                                         = Vector[X]
//   type Elem                                          = Char
//   private def chars(xs: String)                      = xs.toCharArray
//   def hasMap[A](xs: String)(f: Char => A): Vector[A] = xs.toCharArray map f toVector
//   def hasFilter(xs: String)(p: Char => Boolean)      = chars(xs) filter p mkString ""
//   def hasContains(xs: String)(x: Char)               = chars(xs) contains x
//   def hasForeach(xs: String)(f: Char => Unit)        = chars(xs) foreach f
//   def hasIndex(xs: String)(index: std.Index)         = xs charAt index.value
//   def hasSize(xs: String)                            = Size(xs.length)
// }
// implicit final class IsIndex[R](val xs: R)(implicit val v: Is.Index[R]) {
//   def elemAt(index: Index)            = v.hasIndex(xs)(index)
//   def size: Size                      = v.hasSize(xs)
//   def exclusiveEnd: Index             = size.toIndex
//   def hasIndex(index: Index): Boolean = indexRange contains index
//   def indexRange: IndexRange          = size.toIndexRange
//   def index(elem: v.Elem): Index      = indexRange find (i => elemAt(i) == elem)
//   def lastIndex(elem: v.Elem): Index  = indexRange findReverse (i => elemAt(i) == elem)
// }
// implicit final class DoesSizeInfo[R](val xs: R)(implicit v: Has.SizeInfo[R]) {
//   def sizeInfo: SizeInfo = v hasSizeInfo xs
// }
// implicit final class IsMap[R](val xs: R)(implicit val v: Is.Map[R]) {
//   def apply(k: v.In): v.Out            = v.hasApply(xs)(k)
//   def keysVector: Vector[v.In]         = (Vector.newBuilder[v.In] doto (b => v.hasForeach(xs)(b += _))).result
// }

// implicit final class IsIterable[R](val xs: R)(implicit val v: Is.Iterable[R]) {
//   def hasForeach(xs: R)(f: v.Elem => Unit): Unit     = (v hasForeach xs)(f)
//   def hasSizeInfo(xs: Foreach[v.Elem]): std.SizeInfo = xs.sizeInfo
// }

// final class IsVector[A] extends Is.Index[Vector[A]] with Has.Contains[Vector[A]] with Has.Map[Vector[A]] with Has.Filter[Vector[A]] {
//   type CC[X]                                         = Vector[X]
//   type Elem                                          = A
//   def hasMap[B](xs: Vector[A])(f: A => B): Vector[B] = xs map f
//   def hasFilter(xs: Vector[A])(p: A => Boolean)      = xs filter p
//   def hasContains(xs: Vector[A])(x: A)               = xs contains x
//   def hasForeach(xs: Vector[A])(f: A => Unit)        = xs foreach f
//   def hasIndex(xs: Vector[A])(index: std.Index)      = xs(index.value)
//   def hasSize(xs: Vector[A])                         = Size(xs.length)
// }
