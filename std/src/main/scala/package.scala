package psp

package object std extends psp.std.PackageLevel {
  implicit class ArrayViewOps[A](val repr: Array[A]) {
    def m: IndexedView[A, Array[A]] = new DirectAccess.ArrayIs[A] wrap repr
  }
  implicit class StringViewOps[A](val repr: String) {
    def m: IndexedView[Char, String] = DirectAccess.StringIs wrap repr
  }
}
