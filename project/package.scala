package psp

import sbt._, Keys._
import sbt.complete.Parser

package object build {
  type ParserOf[A]    = Def.Initialize[State => Parser[A]]
  type SettingOf[A]   = Def.Initialize[A]
  type TaskOf[A]      = Def.Initialize[Task[A]]
  type InputTaskOf[A] = Def.Initialize[InputTask[A]]
  type SettingSeq     = Seq[Setting[_]]
  type ScopedKeySeq   = Seq[Def.ScopedKey[_]]
  type UnscopedKeySeq = Seq[AttributeKey[_]]
  type DirToFiles     = File => PathFinder
  type DependsRef     = ClasspathDep[ProjectReference]

  implicit def implicitStateOps(s: State): Stated = new Stated(s)

  def doto[A](x: A)(f: A => Unit): A = try x finally f(x)

  def dateTime(): String = new java.text.SimpleDateFormat("yyyyMMdd-HH-mm-ss") format new java.util.Date

  def stateCommand(f: (State, List[String]) => Unit): (State, Seq[String]) => State =
    (state, args) => doto(state)(s => f(s, args.toList))
}
