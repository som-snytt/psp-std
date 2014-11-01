package psp
package std

import api._

object Pair {
  def apply[R, A, B](x: A, y: B)(implicit z: PairUp[R, A, B]): R          = z.create(x, y)
  def unapply[R, A, B](x: R)(implicit z: PairDown[R, A, B]): Some[(A, B)] = Some((z left x, z right x))
}
object Split {
  def apply[A](left: View[A], right: View[A]): View.Split[A] = SplitView(left, right)
  def unapply[A](x: View.Split[A]): Some[(View[A], View[A])] = Some(x.left -> x.right)
}
