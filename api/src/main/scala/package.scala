package psp
package std

package object api {

}

package api {
  trait Labeled {
    def label: String
    override def toString = label
  }
}
