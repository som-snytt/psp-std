package psp
package std

import api._

package jvm {
  object opcodes {
    private def codes(p: Predicate[Opcode]): Direct[Opcode] = Opcode.values filter p toDirect

    def all        = Opcode.values.toDirect
    def constant   = codes(_.isConstant)
    def load       = codes(_.isLoad)
    def store      = codes(_.isStore)
    def stack      = codes(_.isStack)
    def math       = codes(_.isMath)
    def conversion = codes(_.isConversion)
    def comparison = codes(_.isComparison)
    def control    = codes(_.isControl)
    def reference  = codes(_.isReference)
    def extended   = codes(_.isExtended)
    def reserved   = codes(_.isReserved)
  }
}

package object jvm {
  implicit class OpcodeOps(val op: Opcode) extends AnyVal {
    private def within(lo: Int, hi: Int): Boolean = lo <= op.getValue && op.getValue <= hi

    def isConstant   = within(0, 20)
    def isLoad       = within(21, 53)
    def isStore      = within(54, 86)
    def isStack      = within(87, 95)
    def isMath       = within(96, 132)
    def isConversion = within(133, 147)
    def isComparison = within(148, 166)
    def isControl    = within(167, 177)
    def isReference  = within(178, 195)
    def isExtended   = within(196, 201)
    def isReserved   = within(202, 255)
  }
}
