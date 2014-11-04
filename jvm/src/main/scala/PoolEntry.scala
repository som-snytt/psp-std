package psp
package std
package jvm

import JvmConstants._

/*
  4.2.2 Unqualified Names
  Names of methods, fields and local variables are stored as unqualified
  names. Unqualified names must not contain the characters '.', ';', '['
  or '/'. Method names are further constrained so that, with the exception
  of the special method names <init> and <clinit> (§3.9), they must not
  contain the characters '<' or '>'.

  4.3 Descriptors and Signatures
  A descriptor is a string representing the type of a field or method.
  Descriptors are represented in the class file format using modified
  UTF-8 strings (§4.4.7) and thus may be drawn, where not further
  constrained, from the entire Unicode character set. A signature is a
  string representing the generic type of a field or method, or generic
  type information for a class declaration.
*/
sealed trait PoolEntry
object PoolEntry {
  implicit def poolEntryOps(p: PoolEntry): pool.PoolEntryOps = new pool.PoolEntryOps(p)
}

package pool {
  sealed trait DirectEntry extends PoolEntry { def value: Any }
  sealed trait Name_Info extends PoolEntry { def name_index: UShort }
  sealed trait Ref_Info extends PoolEntry  { def class_index: UShort ; def name_and_type_index: UShort }
  object Ref_Info {
    def unapply(x: Ref_Info) = Some((x.class_index, x.name_and_type_index))
  }

  final case class Fieldref_info(class_index: UShort, name_and_type_index: UShort)           extends Ref_Info
  final case class InterfaceMethodref_info(class_index: UShort, name_and_type_index: UShort) extends Ref_Info
  final case class Methodref_info(class_index: UShort, name_and_type_index: UShort)          extends Ref_Info
  final case class NameAndType_info(name_index: UShort, descriptor_index: UShort)            extends Name_Info
  final case class Class_info(name_index: UShort)                                            extends Name_Info
  final case class String_info(string_index: UShort)                                         extends PoolEntry
  final case class Double_info(value: Double)                                                extends DirectEntry
  final case class Float_info(value: Float)                                                  extends DirectEntry
  final case class Integer_info(value: Int)                                                  extends DirectEntry
  final case class Long_info(value: Long)                                                    extends DirectEntry
  final case class Utf8_info(value: String)                                                  extends DirectEntry

  final case object NoEntry extends PoolEntry { }

  final class PoolEntryOps(val entry: PoolEntry) extends AnyVal {
    def label: String = if (entry eq null) "<null>" else entry.shortClass stripSuffix "_info"
    def content: String = entry match {
      case x: DirectEntry      => "" + x.value
      case x: Ref_Info         => "#%s.#%s".format(x.class_index.toInt, x.name_and_type_index.toInt)
      case x: NameAndType_info => "#%s:#%s".format(x.name_index.toInt, x.descriptor_index.toInt)
      case Class_info(index)   => "#" + index.toInt
      case String_info(index)  => "#" + index.toInt
      case _                   => ""
    }
    def width: Int = (tag: @switch) match {
      case CONSTANT_Double | CONSTANT_Long => 2
      case _                               => 1
    }
    def tag: Byte = entry match {
      case _: Fieldref_info           => CONSTANT_Fieldref
      case _: InterfaceMethodref_info => CONSTANT_InterfaceMethodref
      case _: Methodref_info          => CONSTANT_Methodref
      case _: Class_info              => CONSTANT_Class
      case _: NameAndType_info        => CONSTANT_NameAndType
      case _: String_info             => CONSTANT_String
      case _: Double_info             => CONSTANT_Double
      case _: Float_info              => CONSTANT_Float
      case _: Integer_info            => CONSTANT_Integer
      case _: Long_info               => CONSTANT_Long
      case _: Utf8_info               => CONSTANT_Utf8
      case _                          => -1
    }
  }
}
