package psp
package std

sealed trait Nat extends api.Nat {
  type This <: Nat
  type Prev <: Nat { type Succ <: This }
  type Succ <: Nat { type Prev <: This }
}

// Meaning Nat[ Prev This Succ ]
sealed trait NatPTS[P <: Nat, T <: Nat, S <: Nat] extends Nat {
  type This = T
  type Prev = P { type Succ = This }
  type Succ = S { type Prev = This }
}

object Nat {
  final class _0 extends NatPTS[Nothing, _0, _1]
  final class _1 extends NatPTS[_0, _1, _2]
  final class _2 extends NatPTS[_1, _2, _3]
  final class _3 extends NatPTS[_2, _3, _4]
  final class _4 extends NatPTS[_3, _4, _5]
  final class _5 extends NatPTS[_4, _5, _6]
  final class _6 extends NatPTS[_5, _6, _7]
  final class _7 extends NatPTS[_6, _7, _8]
  final class _8 extends NatPTS[_7, _8, _9]
  final class _9 extends NatPTS[_8, _9, ??]
  final class ?? extends NatPTS[??, ??, ??]

  final val _0: _0  = new _0
  final val _1: _1  = new _1
  final val _2: _2  = new _2
  final val _3: _3  = new _3
  final val _4: _4  = new _4
  final val _5: _5  = new _5
  final val _6: _6  = new _6
  final val _7: _7  = new _7
  final val _8: _8  = new _8
  final val _9: _9  = new _9
  final val ?? : ?? = new ??
}

// Attempts to use Nat.this.type as refinements of the abstract
// successor and predecessor types run into compilation time
// exponential explosion. Timing notes:
//
// Largest source alias is _6
// Largest existential created is _3220.type
// 3.555 real, 7.164 user, 0.202 sys
//
// Largest source alias is _7
// Largest existential created is _22432.type
// 6.056 real, 11.557 user, 0.416 sys
//
// Largest source alias is _8
// Largest existential created is _156892.type
// 20.183 real, 29.146 user, 1.645 sys
//
// Largest source alias is _9
// Largest existential created is _1098088.type
// 869.508 real, 902.972 user, 16.297 sys
