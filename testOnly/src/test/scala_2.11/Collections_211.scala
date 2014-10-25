package psp
package tests

import psp.std._
import StdShow._

class Collections_211 extends ScalacheckBundle {
  def bundle = "Collections, 2.11 only"

  def props: sciList[NamedProp] = sciList(
    seqShows("10 -> 2, 30 -> 4", view(1 -> 2, 3 -> 4) mapLeft (_ * 10))
  )
}
