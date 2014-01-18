// package psp
// package core

// import Foreach._


// final case class PspView[A](xs: Foreach[A]) extends Foreach[A] with Viewable[A] {
//   type CC[X] = PspView[X]
//   def sizeInfo = xs.sizeInfo

//   @inline final def foreach(f: A => Unit): Unit = xs foreach f

//   // xs match {
//   //   SliceByIndex(xs, start, Some(end))

//   //   case xs: Indexed[_] => xs foreach f
//   //   case _              => xs foreach f
//   // }

//   def map[B](f: A => B): CC[B]                   = Mapped(xs, f)
//   def flatMap[B](f: A => Foreach[B]): CC[B]      = FlatMapped(xs, f)
//   def collected[A1 >: A, B](pf: A1 =?> B): CC[B] = Collected(xs, pf)
//   def ++(ys: Foreach[A]): Self = Joined(xs, ys)
//   // def ++[A1 >: A](ys: Foreach[A1]): CC[A1]       = Joined(xs, ys)
//   def filter(p: A => Boolean): Self              = Filtered(xs, p)
//   // def drop(n: Int): Self                         = Dropped(xs, n)

//   def drop(n: Int): Self = xs match {
//     case Mapped(xs: Indexed[A], f) => Mapped(xs.slice(n, xs.size.value), f)
//     case xs: Indexed[A]            => xs.slice(n, xs.size.value)
//     case _                         => Dropped(xs, n)
//   }
//   def take(n: Int): Self = xs match {
//     case Mapped(xs: Indexed[A], f) => Mapped(xs.slice(0, n), f)
//     case xs: Indexed[A]            => xs.slice(0, n)
//     case _                         => Taken(xs, n)
//   }
//   def slice(start: Int, end: Int): Self = {
//     ( if (start < 0) slice(0, end)
//       else if (end <= start) Empty
//       else xs match {
//         case Mapped(xs: SliceByIndex[A], f) => Mapped(xs.slice(start, end), f)
//         case xs: SliceByIndex[A]            => xs.slice(start, end)
//         case xs: Indexed[A]                 => SliceByIndex(xs, start, Some(end))
//         case _                              => drop(start) take (end - start)
//       }
//     )
//   }

//   // private def sliceInternal[A](xs: Indexed[A], start: Int, end: Int): Self = xs match {
//   //   case SliceByIndex(xs0, start0, Some(end0)) => SliceByIndex(xs0, start0 + start, Some((start0 + end) min (end0 - start0 - start)))
//   //   case _                                     => SliceByIndex(xs, start, Some(end))
//   // }

//       // case xs: SliceByIndex[_] => xs.slice(start, end)

//   //     case SliceByIndex(underlying, s, e) =>

//   //   // else drop(start) take (end - start)
//   // )

//   // // (
//   //   if (start < 0) slice(0, end)
//   //   else if (end <= start) Empty
//   //   else xs match {
//   //     case SliceByIndex(underlying, start1, end1) =>
//   //       val nextSize = (xs.sizeInfo - start) min precise(end - start)

//   //       //  min precise(end - start)

//   //       // val oldSize = end1 - start1
//   //       // val maxSize = (oldSize - start) min (end - start)
//   //       // val size = maxSize min (end1 - start1)
//   //       // val s = start + start1
//   //       // SliceByIndex(underlying, s, s + size)
//   //     case _ =>
//   //       SliceByIndex(xs, start, end)
//   //   }
//   // )

//   override def toString = ss"$xs"
// }

// // object PspView {
// //   // lazy val Empty = apply(Foreach.Empty)
// //   // implicit def foreachToView[A](xs: Foreach[A]): PspView[A] = apply(xs)

// //   // def apply[A](): PspView[A] = Empty
// // }
