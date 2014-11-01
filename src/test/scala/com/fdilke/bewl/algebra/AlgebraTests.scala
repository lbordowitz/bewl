package com.fdilke.bewl.algebra

import com.fdilke.bewl.fsets.NativeFiniteSets._
import com.fdilke.bewl.fsets.NativeFiniteSetsUtilities._
import com.fdilke.bewl.fsets.{NativeFiniteSets, NativeFiniteSetsUtilities}
import com.fdilke.bewl.testutil.RunTimeCompilation
import com.fdilke.bewl.topos.StarTag.Principal
import org.scalatest.FunSpec
import org.scalatest.Matchers._

class AlgebraTests extends FunSpec with RunTimeCompilation {

//  describe("Arities") {
//    it("have a computed type") {
//      Arity().calcType.TYPE
//    }
//  }

  describe("Abstract operators") {
    it("are typed so that they can be combined only with matching arities and return values") {
      inContextOf(imports = Seq("com.fdilke.bewl.fsets.NativeFiniteSets.AbstractOp._")) {
        "multiply(unit, unit)" should compile
        "rightScalarMultiply(unit, unitRightScalar)" should compile

        "multiply(unit)" should not (compile)
        "multiply(unit, unitRightScalar)" should not (compile)
        "rightScalarMultiply(unit, unit)" should not (compile)
      }
    }
  }

  describe("Algebraic theories") {

    it("can be defined and used to create instances") {
      // TODO: aspirational: should eventually be able to do this, and actions: test drive other machinery first
      //        val monoids = new AlgebraicTheory(
      //          operators = Set(AbstractOp.unit, AbstractOp.multiply),
      //          laws = Seq(
      //            Law.leftUnit(AbstractOp.unit, AbstractOp.multiply),
      //            Law.rightUnit(AbstractOp.unit, AbstractOp.multiply),
      //            Law.associative(AbstractOp.multiply)
      //          ))
      //        val (i, x, y) = ('i, 'x, 'y)
      //        val carrier = makeStar(i, x, y)
      //        val unit = makeNullaryOperator(carrier, i)
      //        val product = makeBinaryOperator(carrier,
      //          (i, i) -> i, (i, x) -> x, (i, y) -> y,
      //          (x, i) -> x, (x, x) -> x, (x, y) -> x,
      //          (y, i) -> y, (y, x) -> y, (y, y) -> y
      //        )
      //        monoids[Symbol](carrier, AbstractOp.unit := unit, AbstractOp.multiply := product).sanityTest
    }

  }

  describe("Algebraic laws") {
    it("can be expressed by building identities from abstract operators and constants") {
      import AbstractOp._

      val leftUnitLaw = Law("not a left unit", (x : Variable[Principal]) =>
        multiply(unit, x) ::== x
      )

      val magmasWith1 = AlgebraicTheory(
        operators = Seq(unit, multiply),
        laws = Seq(leftUnitLaw)
      )

      val carrier = makeStar(1, -1)
      def makeUnit(i: Int) = makeNullaryOperator(carrier, i)
      val product = makeBinaryOperator(carrier,
        (1, 1) -> 1, (1, -1) -> -1, (-1, 1) -> -1, (-1, -1) -> 1
      )
      val goodMagmaWith1 = magmasWith1(carrier, unit := makeUnit(1), multiply := product)
      goodMagmaWith1.sanityTest

      if (false) {        // TODO: fix
        val badMagmaWith1 = magmasWith1(carrier, unit := makeUnit(-1), multiply := product)
        intercept[IllegalArgumentException] {
          badMagmaWith1.sanityTest
        }.getMessage shouldBe "Left unit law failed"
      }
    }

    it("can be verified in the context of an algebraic theory, for a specified algebra") {
    }
  }
}
