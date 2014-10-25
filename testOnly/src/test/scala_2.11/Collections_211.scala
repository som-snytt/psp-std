package psp
package tests

import psp.std._, api._
import org.scalacheck._, Prop._
import StdEq._

class Collections_211 extends ScalacheckBundle {
  def bundle = "Collections, 2.11 only"

  def props: sciList[NamedProp] = sciList(
    // seqShows("10 -> 2, 30 -> 4", pSeq(1 -> 2, 3 -> 4).m mapLeft (_ * 10))
  )
}
