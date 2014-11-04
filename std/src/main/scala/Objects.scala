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
object +: {
  def unapply[A](xs: Array[A])       = if (xs.length == 0) None else Some(xs(0) -> (xs drop 1))
  def unapply[A](xs: Each[A])        = xs match { case Each(hd, _*) => Some(hd -> (xs drop 1)) ; case _ => None }
  def unapply[A](xs: sCollection[A]) = if (xs.isEmpty) None else Some(xs.head -> xs.tail)
}


object Logic {
  object quantifier {
    val universal   = "∀"
    val existential = "∃"
    val uniqueness  = "∃!"
  }
  object connective {
    val tautology     = "⊤"
    val contradiction = "⊥"
    val negation      = "¬"
    val implication   = "→" // "⊃"
    val conjunction   = "∧" //
    val disjunction   = "∨" //
    val biconditional = "↔" // "⇔" "≡"

    val xor  = "⊻"
    val nand = "↑"
    val nor  = "↓"
  }
  val definition = ":="
  val derives    = "⊢" // syntactic consequence
  val entails    = "⊨" // semantic consequence
}
