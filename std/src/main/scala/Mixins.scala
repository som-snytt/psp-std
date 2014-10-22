package psp
package std

trait AndThis {
  def andThis(x: Unit): this.type = this
}
