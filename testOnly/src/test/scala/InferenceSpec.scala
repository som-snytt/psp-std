package psp
package tests

import psp.std._, api._

class InferenceSpec extends ScalacheckBundle {
  def bundle = "Inference"

  val as: Array[Int]     = Array(1, 2, 3)
  val ss: String         = "123"
  val ls: sciList[Int]   = sciList(1, 2, 3)
  val vs: sciVector[Int] = sciVector(1, 2, 3)
  val fs: Foreach[Int]   = Foreach.elems(1, 2, 3)
  val ds: Direct[Int]    = Direct.elems(1, 2, 3)

  def expectType[A: CTag](body: A): NamedProp = body.getClass |> (c => "With pt=Any, expected %-10s  actual %s".format(classOf[A].shortName, c.shortName) -> Prop(classOf[A] isAssignableFrom c))

  def props: Seq[NamedProp] = Seq(
    expectType[Foreach[Int]](fs map identity),
    expectType[View[Int]](fs.m map identity),
    expectType[Foreach[Int]](fs.m map identity force),
    expectType[Array[Int]]((fs.m map identity).force[Array[Int]]),
    expectType[Direct[Int]](ds map identity),
    expectType[View[Int]](ds.m map identity),
    expectType[Direct[Int]](ds.m map identity force),
    expectType[Array[Int]]((ds.m map identity).force[Array[Int]]),
    expectType[Array[Int]](as map identity),
    expectType[View[Int]](as.m map identity),
    expectType[Array[Int]](as.m map identity force),
    expectType[Array[Int]]((as.m map identity).force[Array[Int]]),
    expectType[String](ss map identity),
    expectType[View[Char]](ss.m map identity),
    expectType[String](ss.m map identity force),
    expectType[Array[Char]]((ss.m map identity).force[Array[Char]]),
    expectType[sciList[Int]](ls map identity),
    expectType[View[Int]](ls.m map identity),
    expectType[sciList[Int]](ls.m map identity force),
    expectType[Array[Int]]((ls.m map identity).force[Array[Int]]),
    expectType[sciVector[Int]](vs map identity),
    expectType[View[Int]](vs.m map identity),
    expectType[Direct[Int]](vs.m map identity force),
    expectType[Array[Int]]((vs.m map identity).force[Array[Int]])
  )
}
