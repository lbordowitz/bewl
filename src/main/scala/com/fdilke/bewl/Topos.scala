package com.fdilke.bewl

import com.fdilke.bewl.algebra.AbstractOperator
import com.fdilke.bewl.algebra.AlgebraicStructure._
import com.fdilke.bewl.helper.ResultStore
import scala.Function.tupled

trait Topos {
  type DOT[P] <: Dot[P]
  type ARROW[P, Q] <: Arrow[P, Q]
  type BIPRODUCT[P, Q] <: Biproduct[P, Q]
  type EXPONENTIAL[P, Q] <: Exponential[P, Q]
  type EQUALIZER[M, T] <: Equalizer[M, T]

  type EQUALIZER_SOURCE[M, T]
  type TERMINAL
  type OMEGA

  val I: DOT[TERMINAL]
  val omega: DOT[OMEGA]
  val truth: ARROW[TERMINAL, OMEGA]

  type Operator[X] = ARROW[Power[X], X]

  trait Dot[X] {
    def identity: ARROW[X, X]

    def toI: ARROW[X, TERMINAL]

    def multiply[Y](that: DOT[Y]): BIPRODUCT[X, Y]

    final def *[Y](that: DOT[Y]) = standardProducts(
      (this.asInstanceOf[DOT[Any]],
       that.asInstanceOf[DOT[Any]])).asInstanceOf[BIPRODUCT[X, Y]]

    final def x[Y](that: DOT[Y]) = (this * that).product

    def exponential[S](that: DOT[S]): EXPONENTIAL[S, X]

    final def A[S](that: DOT[S]) = standardExponentials(
      (this.asInstanceOf[DOT[Any]],
        that.asInstanceOf[DOT[Any]])).asInstanceOf[EXPONENTIAL[S, X]]

    final def ^[S](that: DOT[S]): DOT[S => X] = (this A that).exponentDot

    final def A[S](exponent: Int) = standardPowers(
      this.asInstanceOf[DOT[Any]],
      exponent).asInstanceOf[IntegerPower[X]]

    final def ^(exponent: Int): DOT[Power[X]] = (this A exponent).power

    final def toPower(exponent: Int): IntegerPower[X] = exponent match {
      case 0 => IntegerPower[X](I, Seq.empty)
      case 1 => IntegerPower[X](this, Seq(this.identity))
      case _ =>
        val xN_1 = this A (exponent - 1)
        val product = this * xN_1.power
        IntegerPower[X](product.product,
          product.leftProjection +: xN_1.projection.map(_(product.rightProjection))
        )
    }
  }

  trait Arrow[X, Y] {
    val source: DOT[X]
    val target: DOT[Y]

    def apply[W](arrow: ARROW[W, X]): ARROW[W, Y]

    final def x[Z](that: ARROW[X, Z]) = (this.target * that.target).
      multiply(this.asInstanceOf[ARROW[X, Y]], that)

    def ?=(that: ARROW[X, Y]): EQUALIZER[X, Y]
    val chi: Characteristic[X, Y]
    def sanityTest: Unit
  }

  trait Biproduct[X, Y] {
    val product: DOT[(X, Y)]
    val leftProjection: ARROW[(X, Y), X]
    val rightProjection: ARROW[(X, Y), Y]

    def multiply[W](leftArrow: ARROW[W, X],
                    rightArrow: ARROW[W, Y]): ARROW[W, (X, Y)]
  }

  trait Exponential[S, T] {
    val exponentDot: DOT[S => T]
    val evaluation: BiArrow[S => T, S, T]
    def transpose[W](multiArrow: BiArrow[W, S, T]): ARROW[W, S => T]
  }

  case class BiArrow[L, R, T](left: DOT[L], right: DOT[R], arrow: ARROW[(L, R), T])
  abstract class Characteristic[X, Y] {
    val arrow: ARROW[Y, OMEGA]
    def restrict[W](arrow: ARROW[W, Y]): ARROW[W, X]
  }

  trait Equalizer[M, T] {
    val equalizerSource: DOT[EQUALIZER_SOURCE[M, T]]
    val equalizer: ARROW[EQUALIZER_SOURCE[M, T], M]
    def restrict[S](equalizingArrow: ARROW[S, M]): ARROW[S, EQUALIZER_SOURCE[M, T]]
  }

  case class IntegerPower[X](_power: Dot[_], _projection: Seq[ARROW[_, _]]) {
    def power = _power.asInstanceOf[DOT[Power[X]]]
    def projection = _projection.asInstanceOf[Seq[Operator[X]]]
  }

  object IntegerPower {
    def multiply[S, X](source: DOT[S], arrows: ARROW[S, X]*): ARROW[S, Power[X]] =
      (arrows size match {
        case 0 => source.toI
        case 1 => arrows(0)
        case _ => arrows.head x multiply(source, arrows.tail: _*)
      }).asInstanceOf[ARROW[S, Power[X]]]
  }

  class Power[X] // just a marker, for now - will have methods as part of the DSL?

  private val standardProducts = new ResultStore[(DOT[Any], DOT[Any]), BIPRODUCT[Any, Any]](tupled {
    (x, y) => x multiply y
  })

  private val standardExponentials = new ResultStore[(DOT[Any], DOT[Any]), EXPONENTIAL[Any, Any]](tupled {
    (x, y) => x exponential y
  })

  private val standardPowers = new ResultStore[(DOT[Any], Int), IntegerPower[Any]](tupled {
    (x, n) => x toPower n
  })

  // Helper methods for biproducts
  def leftProjection[X, Y](x: DOT[X], y: DOT[Y]) = (x * y).leftProjection
  def rightProjection[X, Y](x: DOT[X], y: DOT[Y]) = (x * y).rightProjection

  // Helper methods for triproducts (this could obviously be extended)
  def leftProjection[X, Y, Z](x: DOT[X], y: DOT[Y], z: DOT[Z]) = (x * y).leftProjection(((x x y) * z).leftProjection)
  def midProjection[X, Y, Z](x: DOT[X], y: DOT[Y], z: DOT[Z]) = (x * y).rightProjection(((x x y) * z).leftProjection)
  def rightProjection[X, Y, Z](x: DOT[X], y: DOT[Y], z: DOT[Z]) = ((x x y) * z).rightProjection

  // Helper methods for exponentials
  def evaluation[S, T](s: DOT[S], t: DOT[T]): BiArrow[S=>T, S, T] =
    (t A s).evaluation
  def transpose[S, T, W](s: DOT[S], t: DOT[T], multiArrow: BiArrow[W, S, T]): ARROW[W, S => T] =
    (t A s).transpose(multiArrow)

  // Helper methods for integer powers
  def projection[X](base: DOT[X], exponent: Int, index: Int): Operator[X] =
    (base A exponent).projection(index)

  // Machinery for constructing and verifying algebraic structures with laws (varieties)
  // TODO: is there a way for this stuff to exist outside Topos?

  class Law(
    abstractOperators: Seq[AbstractOperator],
    numVariables: Int,
    _equation: PartialFunction[(Seq[BoundAlgebraicOperator[Power[Nothing], Nothing]], Seq[Operator[Nothing]]), Boolean],
    exceptionMessage: String
  ) {

    def equation[X]: PartialFunction[(Seq[BoundAlgebraicOperator[Power[X], X]], Seq[Operator[X]]), Boolean] =
      _equation.asInstanceOf[PartialFunction[(Seq[BoundAlgebraicOperator[Power[X], X]], Seq[Operator[X]]), Boolean]]

    def verify[X](carrier: DOT[X], operatorMap: Map[AbstractOperator, Operator[X]]) = {
      val power = carrier A numVariables
      val variables = power.projection
      val operators = abstractOperators map { abstractOp =>
        BoundAlgebraicOperator[Power[X], X](power.power, operatorMap(abstractOp))
      }

      val operatorsAndVariables = (operators, variables)
      if (!equation[X](operatorsAndVariables)) {
        throw new IllegalArgumentException(exceptionMessage)
      }
    }
  }
  
  object Law {
    def commutative(aop: AbstractOperator) =
      new Law(Seq(aop), 2, { case (Seq(op), Seq(x, y)) =>
          op(x, y) == op(y, x)
        }, s"Commutative law for operator $aop"
      )

    def associative(aop: AbstractOperator) =
      new Law(Seq(aop), 3,
        { case (Seq(op), Seq(x, y, z)) =>
          op(x, op(y, z)) == op(op(x, y), z) },
        s"Associative law for operator $aop"
      )
    def unit(abstractUnit: AbstractOperator, aop: AbstractOperator) =
      new Law(Seq(abstractUnit, aop), 1,
        { case (Seq(unit, op), Seq(x)) =>
          val kkk: BoundAlgebraicOperator[_, _] = unit
          val u = unit()
          val lll: ARROW[Power[Nothing], Nothing] = u
          println(s"UNIT LAW: u = $u")
          println(s"UNIT LAW: x = $x")
          println(s"UNIT LAW: op(u, x) = ${op(u, x)}")
          op(u, x) == x && op(x, u) == x },
        s"Unit law for operator $aop with unit $abstractUnit"
      )
  }

  case class BoundAlgebraicOperator[S, X](val source: DOT[S], arrow: Operator[X]) {
    def apply(variables: ARROW[S, X]*): ARROW[S, X] =
      arrow(IntegerPower.multiply(source, variables:_*))
  }

  class AlgebraicStructure[X] (
    val carrier: DOT[X],
    val signature: Signature,
    val operatorMap: Map[AbstractOperator, Operator[X]],
    val laws: Law*) {
    def verify = laws.map { _.verify(carrier, operatorMap) }
  }

  case class Monoid[X](dot: DOT[X], unit: Operator[X], product: Operator[X]) extends AlgebraicStructure[X] (
    carrier = dot,
    signature = MonoidSignature,
    operatorMap = Map(AbstractOperator._1 -> unit, AbstractOperator.* -> product),
    Law.unit(AbstractOperator._1, AbstractOperator.*),
    Law.associative(AbstractOperator.*)
  )
}
