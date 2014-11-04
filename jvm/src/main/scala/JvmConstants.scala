package psp
package std
package jvm

object JavaAccFlag {
  final val Public              = 1 << 0
  final val Private             = 1 << 1
  final val Protected           = 1 << 2
  final val Static              = 1 << 3
  final val Final               = 1 << 4
  final val Super, Synchronized = 1 << 5
  final val Volatile, Bridge    = 1 << 6
  final val Transient, Varargs  = 1 << 7
  final val Native              = 1 << 8
  final val Interface           = 1 << 9
  final val Abstract            = 1 << 10
  final val Strict              = 1 << 11
  final val Synthetic           = 1 << 12
  final val Annotation          = 1 << 13
  final val Enum                = 1 << 14
}

object JvmConstants {
  final val JAVA_MAGIC = 0xCAFEBABE

  final val CONSTANT_NoEntry            = 0
  final val CONSTANT_Utf8               = 1
  final val CONSTANT_Integer            = 3
  final val CONSTANT_Float              = 4
  final val CONSTANT_Long               = 5
  final val CONSTANT_Double             = 6
  final val CONSTANT_Class              = 7
  final val CONSTANT_String             = 8
  final val CONSTANT_Fieldref           = 9
  final val CONSTANT_Methodref          = 10
  final val CONSTANT_InterfaceMethodref = 11
  final val CONSTANT_NameAndType        = 12
  final val CONSTANT_MethodHandle       = 15
  final val CONSTANT_MethodType         = 16
  final val CONSTANT_InvokeDynamic      = 18

  import JavaAccFlag._
  import scala.reflect.internal.{ Flags => F }

  private def translateFlags(jflags: Flags, baseFlags: Long, isClass: Boolean): Long = {
    def translateFlag(jflag: Int): Long = (jflag: @switch) match {
      case Private   => F.PRIVATE
      case Protected => F.PROTECTED
      case Final     => F.FINAL
      case Synthetic => F.SYNTHETIC
      case Static    => F.STATIC
      case Abstract  => if (jflags(Annotation)) 0L else if (isClass) F.ABSTRACT else F.DEFERRED
      case Interface => if (jflags(Annotation)) 0L else F.TRAIT | F.INTERFACE | F.ABSTRACT
      case _         => 0L
    }
    var res: Long = F.JAVA | baseFlags
    /** fast, elegant, maintainable, pick any two... */
    res |= translateFlag(jflags.value & Private)
    res |= translateFlag(jflags.value & Protected)
    res |= translateFlag(jflags.value & Final)
    res |= translateFlag(jflags.value & Synthetic)
    res |= translateFlag(jflags.value & Static)
    res |= translateFlag(jflags.value & Abstract)
    res |= translateFlag(jflags.value & Interface)
    res
  }

  def toScalaMethodFlags(flags: Flags): Long = translateFlags(flags, if (flags(Bridge)) F.BRIDGE else 0, isClass = false)
  def toScalaClassFlags(flags: Flags): Long  = translateFlags(flags, 0L, isClass = true)
  def toScalaFieldFlags(flags: Flags): Long  = translateFlags(flags, if (flags(Final)) 0 else F.MUTABLE, isClass = false)
}
