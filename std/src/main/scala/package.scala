package psp

package object std extends psp.std.Implicits {
  val NoIndex = Index.empty
  val NoNth   = Nth.empty

  type MutableSeq[A]                       = scala.collection.mutable.Seq[A]
  def mutableSeq[A](xs: A*): MutableSeq[A] = scala.collection.mutable.Seq(xs: _*)

  implicit class ShowOps[A](val x: A) extends AnyVal {
    def to_s(implicit shows: Show[A]): String = shows show x
  }
}
