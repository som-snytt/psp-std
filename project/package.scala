package psp

import sbt._, Keys._

package object build extends psp.build.Versioning {
  implicit def implicitStateOps(s: State): Stated = new Stated(s)

  def doto[A](x: A)(f: A => Unit): A = try x finally f(x)

  def pspOrg               = "org.improving"
  def targetReleaseVersion = "0.1.0"
  def lastMilestone        = 2
  def isRelease            = sys.props contains "psp.release"
  def isMilestone          = sys.props contains "psp.milestone"

  def stateCommand(f: (State, List[String]) => Unit): (State, Seq[String]) => State =
    (state, args) => doto(state)(s => f(s, args.toList))
}
