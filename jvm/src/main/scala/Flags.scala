package psp
package std
package jvm

import psp.std.scalac.token.Keyword

final case class Flags(value: UShort) extends AnyVal

object Flags {
  implicit class FlagsOps(f: Flags) {
    import f.value
    import java.lang.reflect.{ Modifier => M }
    def apply(f: Int): Boolean = (value & f) != 0

    import JavaAccFlag._

    def isAbstract  = apply(Abstract)
    def isFinal     = apply(Final)
    def isPrivate   = apply(Private)
    def isProtected = apply(Protected)
    def isPublic    = apply(Public)
    def isStatic    = apply(Static)
    def isSynthetic = apply(Synthetic)
    def isInterface = apply(Interface)
    def isAnnotated = apply(Annotation)
    def isVarargs   = apply(Varargs)
    def isBridge    = apply(Bridge)
    def isEnum      = apply(Enum)
    def isTransient = apply(Transient)

    def methodFlags      = Flags(value & M.methodModifiers toChar)
    def constructorFlags = Flags(value & M.constructorModifiers toChar)
    def classFlags       = Flags(value & M.classModifiers toChar)
    def fieldFlags       = Flags(value & M.fieldModifiers toChar)
    def interfaceFlags   = Flags(value & M.interfaceModifiers toChar)
    // def parameterFlags   = compat.parameterFlags(this)

    def modifierString = M toString value
    def classKeyword   = if (isInterface) Keyword.Interface else Keyword.Class

    // override def toString = modifierString
  }
}
