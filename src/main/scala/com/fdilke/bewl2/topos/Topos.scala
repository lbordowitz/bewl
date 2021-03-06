package com.fdilke.bewl2.topos

trait Topos[DOT[_]] {
  val name: String = getClass.getSimpleName

  def sanityTest[S: DOT]: Unit
  def sanityTest[S: DOT, T:DOT](arrow: S => T): Unit

  def compareFunctions[S:DOT, T:DOT](func: S=> T, func2: S => T): Boolean

//  abstract class Equalizer[
//    S: DOT,
//    R <: S : DOT
//  ] {
//    implicit val dot: Any = Topos.this.dot[R]
//    def restrict[Q: DOT](
//      function: Q => S
//    ): Q => R
//  }
  trait Restrictor[S, R <: S] {
    def apply[Q: DOT](arrow: Q => S): Q => R
  }

  abstract class EqualizerReceiver[S: DOT, X] {
    def apply[R <: S : DOT](
      restrictor: Restrictor[S, R]
    ): X
  }

  def equalize[S:DOT, T:DOT, X](
    func1: S => T,
    func2: S => T
  ): EqualizerReceiver[S, X] => X

  // TODO: put this helper code in a separate class

  @inline implicit class RichFunction[S: DOT, T:DOT](
    function: S => T
  ) {
    @inline def =?=(function2: S => T): Boolean =
      compareFunctions(function, function2)

    @inline def ?=[X](function2: S => T): EqualizerReceiver[S, X] => X =
      equalize(function, function2)

    @inline def o[R: DOT](function2: R => S): R => T =
      function compose function2
  }

  // anticipate these will not be used very much...
  // as it's all baked into the types and thoughtcrime is impossible. remove?
  @inline final def dot[S:DOT]: DOT[S] =
    implicitly[DOT[S]]

  final def source[S:DOT, T:DOT](arrow: S => T): DOT[S] =
    dot[S]
  final def target[S:DOT, T:DOT](arrow: S => T): DOT[T] =
    dot[T]
  final def id[S: DOT]: S => S =
    identity
}
