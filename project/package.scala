package psp

import sbt._, Keys._

package object build extends psp.std.api.PackageLevel {
  def stateCommand(f: (State, List[String]) => Unit): (State, Seq[String]) => State =
    (state, args) => state doto (s => f(s, args.toList))
}
