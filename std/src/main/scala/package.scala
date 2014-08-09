package psp

package object std extends psp.std.Implicits with psp.std.Creators {
  val NoIndex = Index.empty
  val NoNth   = Nth.empty

  type ClassTag[A]            = scala.reflect.ClassTag[A]
  type GenTraversableOnce[+A] = scala.collection.GenTraversableOnce[A]

  implicit class ShowOps[A](val x: A) extends AnyVal {
    def to_s(implicit shows: Show[A]): String = shows show x
  }
}
