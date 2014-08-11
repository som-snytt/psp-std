package psp

import sbt._, Keys._

package object build {
  implicit def implicitStateOps(s: State): Stated = new Stated(s)

  def isRelease = sys.props contains "psp.release"
  def doto[A](x: A)(f: A => Unit): A = try x finally f(x)

  def stateCommand(f: (State, List[String]) => Unit): (State, Seq[String]) => State =
    (state, args) => doto(state)(s => f(s, args.toList))
}
