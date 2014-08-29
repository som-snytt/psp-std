package psp
package std
package api

trait Index extends Any                        { def value: Int                  }
trait Size extends Any                         { def value: Int                  }
trait Invariant[A] extends Any                 { def contains(x: A): Boolean     }
trait Foreach[+A] extends Any with HasSizeInfo { def foreach(f: A => Unit): Unit }
trait Indexed[+A] extends Any with Foreach[A]  { def elemAt(index: Index): A     }
trait Direct[+A] extends Any with Indexed[A]   { def size: Size                  }

trait Linear[+A] extends Any with Foreach[A] {
  type Tail <: Linear[A]
  def isEmpty: Boolean
  def head: A
  def tail: Tail
}
