package com.fdilke.bewl2.topos

import java.util.concurrent.atomic.AtomicInteger

import org.scalatest
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

abstract class GenericToposTests[
  DOT[_]: Topos,
  FOO : DOT,
  BAR : DOT,
  BAZ : DOT
] extends AnyFunSpec {

  val foo2bar : FOO => BAR
  val foo2ImageOfBar : FOO => BAZ
  //  TODO: sort this out
  //  val foobar2baz : BiArrow[
  //    FOO,
  //    BAR,
  //    BAZ
  //  ]
  val monicBar2baz: BAR => BAZ

  private final lazy val foo2baz = foo2ImageOfBar // a convenient alias

  trait EqualizerSituationReceiver[X] {
    def apply[S : DOT, M : DOT, T : DOT](
      equalizerSituation: EqualizerSituation[S, M, T]
    ): X
  }

  def provideEqualizerSituation[X](receiver: EqualizerSituationReceiver[X]): X

  private val topos: Topos[DOT] = implicitly[Topos[DOT]]
  import topos._

  case class EqualizerSituation[
    S : DOT,
    M : DOT,
    T : DOT
  ](
    r: S => M,
    s: M => T,
    t: M => T
  ) {

    def sanityTest : Unit = {
      topos.sanityTest[S]
      topos.sanityTest[M]
      topos.sanityTest[T]

      if (s =?= t) {
        throw new IllegalArgumentException("equalizing two arrows that are already equal!")
      }

      (s o r) ==?== (t o r)
    }
  }

  private implicit class FunctionComparisonHelper[S: DOT, T:DOT](
    function: S => T
  ) {
    def ==?==(function2: S => T) =
      assert(function =?= function2)
  }
  private val foo: DOT[FOO] = implicitly[DOT[FOO]]
  private val bar: DOT[BAR] = implicitly[DOT[BAR]]
  private val baz: DOT[BAZ] = implicitly[DOT[BAZ]]

  describe(s"The fixtures for $name") {
    it("include distinct sane objects") {
      val objects: Set[DOT[_]] = Set(
        foo, bar, baz
      )

      objects should have size 3
      objects foreach { dot: DOT[_] =>
        sanityTest(dot)
      }
    }

    it("include sane arrows whose sources and targets match their names") {
      sanityTest(foo2bar)
      source(foo2bar) shouldBe foo
      target(foo2bar) shouldBe bar

      sanityTest(foo2baz)
      source(foo2baz) shouldBe foo
      target(foo2baz) shouldBe baz

//      foobar2baz.arrow.sanityTest
//      foobar2baz.product shouldBe (foo x bar)
//      foobar2baz.product.left shouldBe foo
//      foobar2baz.product.right shouldBe bar
//      foobar2baz.arrow.source shouldBe (foo x bar)
//      foobar2baz.arrow.target shouldBe baz

      sanityTest(monicBar2baz)
      source(monicBar2baz) shouldBe bar
      target(monicBar2baz) shouldBe baz

      provideEqualizerSituation(new EqualizerSituationReceiver[Unit] {
        def apply[S: DOT, M: DOT, T: DOT](
          equalizerSituation: EqualizerSituation[S, M, T]
        ): Unit =
          equalizerSituation.sanityTest
      })
    }
  }

  describe(s"The topos $name") {

    // TODO: hopefully don't need this nonsense anymore... delete?
//    it("wraps dots and arrows with relatively sane equality semantics") {
//      makeSampleDot().sanityTest
//      makeSampleDot() shouldBe makeSampleDot()
//      (makeSampleDot() eq makeSampleDot()) shouldBe true
//
//      makeSampleArrow().sanityTest
//      makeSampleArrow() shouldBe makeSampleArrow()
//      (makeSampleArrow() eq makeSampleArrow()) shouldBe false
//    }

    it("has identity arrows which can be composed") {
      val identityFoo: FOO => FOO = identity
      id[FOO]  ==?== identityFoo
      (foo2bar o id[FOO])  ==?== foo2bar
      (id[BAR] o foo2bar) ==?== foo2bar
    }

    ignore("has equalizers") {
      // 2 levels of fancy footwork required to extract the types
      provideEqualizerSituation(new EqualizerSituationReceiver[scalatest.Assertion] {
        def apply[S : DOT, M : DOT, T : DOT](
          equalizerSituation: EqualizerSituation[S, M, T]
        ): scalatest.Assertion = {
          import equalizerSituation._
          val numCalls: AtomicInteger =
            new AtomicInteger(0)
          (s ?= t) (
            new EqualizerReceiver[M, Boolean] {
              override def apply[R <: M : DOT](restrictor: Restrictor[M, R]): Boolean = {
                val inclusion: R => M = identity
                (s o inclusion) ==?== (t o inclusion)
                (inclusion o restrictor(r)) ==?== r
                true
              }
            }
          ) shouldBe true
          numCalls.get shouldBe 1
        }
      })
    }

    /*
    it("can construct biproduct diagrams") {
      (bar x baz).sanityTest
      (bar x baz) should have(
        left (bar),
        right (baz)
      )
      val productArrow = foo2bar x foo2baz

      productArrow.sanityTest
      productArrow should have (
        source (foo),
        target (bar x baz),
        sanityTest (null)
      )

      (bar x baz).π0.sanityTest
      (bar x baz).π1.sanityTest

      foo(bar) {
        x => productArrow(x)._1
      } shouldBe foo2bar

      foo(baz) {
        x => productArrow(x)._2
      } shouldBe foo2baz

      val fooXbar: BIPRODUCT[FOO, BAR] =
        foo x bar
      fooXbar(fooXbar) {
        ⊕ tupled fooXbar.pair
      } shouldBe fooXbar.identity
    }

    it("has a terminator") {
      I.sanityTest
      val fooToI = foo.toI
      fooToI.sanityTest
      fooToI.source shouldBe foo
      fooToI.target shouldBe topos.I

      bar.toI o foo2bar shouldBe fooToI
    }

    it("has a (derived) initial object") {
      O.sanityTest
      val fooFromO = foo.fromO
      fooFromO.sanityTest
      fooFromO.source shouldBe O
      fooFromO.target shouldBe foo

      foo2bar o fooFromO shouldBe bar.fromO
      O >> foo shouldBe Seq(fooFromO)
    }

    it("consistently calculates arrows from the initial to the terminal") {
      O.toI shouldBe I.fromO
    }

    it("has standardized products") {
      (foo x bar) shouldBe (foo x bar)
    }

    it("can chain products") {
      val barXfooXbaz = bar x foo x baz
      val productArrow = foo2bar x foo.identity x foo2baz
      productArrow.sanityTest
      productArrow.source shouldBe foo
      productArrow.target shouldBe barXfooXbaz

      leftProjection(bar, foo, baz) o productArrow shouldBe foo2bar
      midProjection(bar, foo, baz) o productArrow shouldBe foo.identity
      rightProjection(bar, foo, baz) o productArrow shouldBe foo2baz
    }

    it("can construct exponential diagrams") {
      // Check evaluation maps baz^bar x bar -> baz
      val exponential = bar > baz
      exponential.sanityTest
      val evaluation = exponential.evaluation
      evaluation.product.sanityTest
      evaluation.product.left shouldBe (bar > baz)
      evaluation.product.right shouldBe bar
      evaluation.arrow.sanityTest
      evaluation.arrow.target shouldBe baz

      val foo2bar2baz: FOO > (BAR → BAZ) =
        (bar > baz) transpose foobar2baz
      foo2bar2baz.sanityTest
      foo2bar2baz should have(
        source(foo),
        target(bar > baz)
      )

      implicit val anonImplicit = bar > baz
      (foo x bar)(baz) {
        case f ⊕ b =>
          foo2bar2baz(f)(b)
      } shouldBe foobar2baz.arrow
    }

    it("has standardized exponentials") {
      (foo > bar) shouldBe (foo > bar)
    }


    it("has a truth object (subobject classifier)") {
      omega.sanityTest
      truth.sanityTest
      truth.source shouldBe I
      truth.target shouldBe omega

      falsity.sanityTest

      val char = monicBar2baz.chi
      char.sanityTest
      char.source shouldBe baz
      char.target shouldBe omega

      char o monicBar2baz shouldBe bar.toTrue

      val restriction = foo2ImageOfBar \ monicBar2baz
      restriction.sanityTest
      restriction.source shouldBe foo
      restriction.target shouldBe bar
      monicBar2baz o restriction shouldBe foo2ImageOfBar

      // Note behaviour is not defined for these pathological cases:
      // construct a non-monic arrow, have chi throw a NotMonicException
      // try backdividing by a monic when we can't
      // It's up to the caller to check. There could be a safe backdivide
    }

    it("expresses the subobject classifier as the carrier of a Heyting algebra") {
      if (!inActionTopos) {
        // reluctantly skip, too slow with current technology
        Ω shouldBe a[HeytingAlgebra[_]]
        Ω.carrier shouldBe omega
        Ω.sanityTest
      }
    }

    it("has enumeration of globals and arrows") {
      I.globals shouldBe Seq(I.identity)
      (foo x baz).globals.size shouldBe foo.globals.size * baz.globals.size
      foo >> I shouldBe Seq(foo.toI)
      foo >> bar should contain(foo2bar)
      foo >> baz should contain(foo2ImageOfBar)
      bar >> baz should contain(monicBar2baz)
    }

    optionalGenerator map { generator =>
      it("has a generator") {
        def distinguishesMapsBetween[
          A <: ~,
          B <: ~
        ] (
            source: DOT[A],
            target: DOT[B]
          ) =
          for {
            anArrow <- source >> target
            anotherArrow <- source >> target if anotherArrow != anArrow
          } {
            generator >> source exists { g =>
              (anArrow o g) != (anotherArrow o g)
            } shouldBe true
          }

        distinguishesMapsBetween(foo, bar)
        distinguishesMapsBetween(bar, baz)
        distinguishesMapsBetween(baz, foo)
      }
    }

    it("can size objects") {
      O should have size 0
      I should have size 1
      foo.size should be > 1
      (foo x bar) should have size (foo.size * bar.size)
      (foo + bar) should have size (foo.size + bar.size)
    }

    it("can tell if an arrow is monic") {

      if (!inActionTopos) { // reluctantly skip, too slow with current technology
        monicBar2baz shouldBe monic

        (foo x foo).π0 should not be monic
        foo.=?=.arrow should not be monic

        foo.diagonal shouldBe monic
        foo.singleton shouldBe monic
      }

      I.identity shouldBe monic
      truth shouldBe monic
      falsity shouldBe monic

      foo.toI should not be monic
      foo.fromO shouldBe monic
    }

    it("can tell if a arrow is epic") {

      I.identity shouldBe epic
      O.identity shouldBe epic
      I.diagonal shouldBe epic
      truth should not be epic
      foo.toI shouldBe epic

      if (!inActionTopos) { // reluctantly skip, too slow with current technology
        foo.identity shouldBe epic
        (foo x foo).π0 shouldBe epic
        foo.diagonal should not be epic
        omega.diagonal should not be epic

        monicBar2baz should not be epic
      }
    }

    it("can tell if an arrow is iso and if so, calculate the inverse") {
      val iI = I.identity
      iI shouldBe iso
      iI.inverse shouldBe iI

      I.diagonal shouldBe iso

      val fooI = foo.identity
      fooI shouldBe iso
      fooI.inverse shouldBe fooI

      I -* foo shouldBe iso
      foo *- I shouldBe iso
    }

    it("can do epic-mono factorizations (images)") {
      val (
        epic,
        monic
        ) : (
        FOO > BAR,
          BAR > BAR
        ) = foo2bar.factorizeEpiMono

      epic shouldBe epic
      monic shouldBe monic
      (monic o epic) shouldBe foo2bar
    }

    if (!inActionTopos) { // reluctantly skip, too slow with current technology
      it("has sane injectives") {
        O should not be injective
        I shouldBe injective
        omega shouldBe injective
      }
    }

    if (imageFinder != DefaultImageFinder)
      it("has a local image finder behaving as default") {
        for {
          s2t <- foo >> bar
        } {
          val defaultImage =
            DefaultImageFinder.image(s2t)
          val localImage =
            imageFinder.image(s2t)

          localImage.equalizerTarget shouldBe
            defaultImage.equalizerTarget

          localImage.restrict(
            defaultImage.inclusion
          ) shouldBe iso

          defaultImage.restrict(
            localImage.inclusion
          ) shouldBe iso
        }
      }

    if (logicalOperations.getClass.getSimpleName != "DefaultLogicalOperations")
      it("has local logical operations behaving as default") {
        val default = new topos.DefaultLogicalOperations

        default.and shouldBe logicalOperations.and
        default.or shouldBe logicalOperations.or
        default.implies shouldBe logicalOperations.implies
        default.falsity shouldBe logicalOperations.falsity
      }

     */
  }
}
