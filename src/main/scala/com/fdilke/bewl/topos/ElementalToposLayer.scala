package com.fdilke.bewl.topos

import com.fdilke.bewl.diagrammatic.BaseDiagrammaticTopos
import com.fdilke.bewl.fsets.FiniteSets._
import com.fdilke.bewl.helper.Memoize

object ElementalToposLayer {
  def apply(
    Δ: BaseDiagrammaticTopos
  ): Topos[Δ.Element] with Wrappings[Δ.Element, Any, Δ.DOT, Δ.ARROW, Δ.WrappedArrow] = {
    class ElementalToposLayer extends Topos[Δ.Element] with Wrappings[
      Δ.Element,
      Any,
      Δ.DOT,
      Δ.ARROW,
      Δ.WrappedArrow
    ] { layer =>

      override type DOT[S <: Δ.Element] = AdapterDot[S]
      override type >[S <: Δ.Element, T <: Δ.Element] = AdapterArrow[S, T]

      override type UNIT = Δ.WrappedArrow[Unit]
      lazy val I: DOT[UNIT] = makeDot(Δ.I).asInstanceOf[DOT[UNIT]]

      override type TRUTH = AdapterTruth

      override lazy val omega = makeDot(Δ.omega).asInstanceOf[DOT[TRUTH]]
      override lazy val truth = makeArrow(Δ.truth).asInstanceOf[UNIT > TRUTH]

      trait AdapterTruth extends Δ.Element

      trait AdapterDot[T <: Δ.Element] extends Dot[T] { self =>
        private[ElementalToposLayer] val dot: Δ.DOT[Any]

        override lazy val toI: T > UNIT =
          AdapterArrow.fromArrow(self, I, dot.toI)

        override lazy val globals: Traversable[UNIT > T] =
          dot.globals.map { global =>
            AdapterArrow.fromArrow(I, self, global)
          }

        override def xUncached[U <: Δ.Element](that: DOT[U]) =
          new AdapterDot[T x U] with BiproductDot[T, U, T x U] {
            override val left = self
            override val right = that

            override def pair(t: T, u: U) =
              asElement(t.arrow x u.arrow)

            override private[ElementalToposLayer] val dot =
              (self.dot x that.dot).asInstanceOf[Δ.DOT[Any]]

            override private[ElementalToposLayer] def asElement(anArrow: Δ.ARROW[_, _]) =
              new (T, U)(
                self.asElement(fletch(Δ.leftProjection(self.dot, that.dot))(fletch(anArrow))),
                that.asElement(fletch(Δ.rightProjection(self.dot, that.dot))(fletch(anArrow)))
              ) with Δ.Element {
                override val arrow: Δ.ARROW[Any, Any] = fletch(anArrow)
              }
          }

        override def `>Uncached`[U <: Δ.Element](that: DOT[U]) =
          new AdapterDot[T → U] with ExponentialDot[T, U, T → U] { exponentialAdapter =>
            override val source = self
            override val target = that

            private val exponential = target.dot A source.dot
            override private[ElementalToposLayer] val dot = 
              exponential.exponentDot.asInstanceOf[Δ.DOT[Any]]

            override def transpose[R <: Δ.Element](biArrow: BiArrow[R, T, U]) =
              AdapterArrow.fromArrow(
                biArrow.product.left, 
                exponentialAdapter,
                exponential.transpose(layer.biArrow(biArrow))
              )

            override private[ElementalToposLayer] def asElement(anArrow: Δ.ARROW[_, _]) =
              new (T => U) with Δ.Element {
                override val arrow: Δ.ARROW[Any, Any] = fletch(anArrow)

                override def apply(s: T): U =
                  target.asElement(
                    Δ.evaluation(source.dot, target.dot)(
                      anArrow.asInstanceOf[Δ.ARROW[Any, Any => Any]],
                      s.arrow
                    ))
              }
          }

        override def sanityTest = dot.sanityTest

        override def apply[U <: Δ.Element](target: DOT[U])(f: T => U) =
          AdapterArrow[T, U](this, target, f)

        private[ElementalToposLayer] def asElement(arrow: Δ.ARROW[_, _]): T

        override def toString: String = s"AdapterDot[$dot]"
      }

      object AdapterArrow {
        def apply[S <: Δ.Element, T <: Δ.Element](
          source: DOT[S], target: DOT[T], function:  S => T
        ) =
          new AdapterArrow[S, T](source, target,
            () => function,
            () => function(source.asElement(source.dot.identity)).arrow
          )

        def fromArrow[S <: Δ.Element, T <: Δ.Element](
          source: DOT[S],
          target: DOT[T],
          arrow: Δ.ARROW[_, _]
        ) =
          new AdapterArrow[S, T](source, target,
            () => t => target.asElement(fletch(arrow)(t.arrow)),
            () => fletch(arrow)
          )
      }

      class AdapterArrow[S <: Δ.Element, T <: Δ.Element](
        val source: DOT[S],
        val target: DOT[T],
        _function: () => S => T,
        _arrow: () => Δ.ARROW[Any, Any]
      ) extends Arrow[S, T] { adapter =>

        private[ElementalToposLayer] lazy val arrow = _arrow()
        private[ElementalToposLayer] lazy val function = _function()

        override def apply(s: S) = function(s)

        override def o[R <: Δ.Element](that: R > S) =
          that.source(target)(function compose that.function)

        def ?=(that: S > T) =
          new AdapterDot[S] with EqualizingDot[S] { equalizingDot =>
            private val equalizer = arrow ?= that.arrow

            override val equalizerTarget = adapter.source

            override private[ElementalToposLayer] val dot = 
              equalizer.equalizerSource.asInstanceOf[Δ.DOT[Any]]

            override private[ElementalToposLayer] def asElement(
              anArrow: Δ.ARROW[_, _]
            ): S =
              new Δ.WrappedArrow(fletch(anArrow)).asInstanceOf[S]

            override def restrict[R <: Δ.Element](arrow: R > S) =
              AdapterArrow.fromArrow(
                arrow.source,
                equalizingDot,
                equalizer.restrict(arrow.arrow)
              )

            // TODO: why do we need to override this?
            // presumably because of the concept of EQUALIZER_SOURCE[_] :(
            override lazy val inclusion: S > S =
              AdapterArrow.fromArrow(
                equalizingDot,
                source,
                equalizer.equalizer
              )
          }

        override def equals(other: Any) = other match {
          case that: (S > T) => arrow == that.arrow
          case _ => false
        }

        override lazy val chi = AdapterArrow.fromArrow(target, omega, arrowChi.arrow)

        override def \[U <: Δ.Element](monic: U > T) =
          AdapterArrow.fromArrow(source, monic.source, monic.arrowChi.restrict(arrow))

        override def sanityTest = {
          if (source.dot != arrow.source) {
            throw new IllegalArgumentException("Source inconsistent")
          }
          if (target.dot != arrow.target) {
            throw new IllegalArgumentException("Source inconsistent")
          }
          arrow.sanityTest
        }

        override def toString: String = s"AdapterArrow[$arrow]"

        private lazy val arrowChi = arrow.chi
      }

      private class WrappedDot[X](innerDot: Δ.DOT[X]) extends DOT[Δ.WrappedArrow[X]] {
        override private[ElementalToposLayer] val dot: Δ.DOT[Any] = innerDot.asInstanceOf[Δ.DOT[Any]]

        override private[ElementalToposLayer] def asElement(arrow: Δ.ARROW[_, _]) =
          new Δ.WrappedArrow(fletch(arrow))
      }

      private val memoizedWrappedDot = {
        def wrapDot[T](dot: Δ.DOT[T]) = new WrappedDot(dot)
        Memoize.generic(wrapDot)
      }

      private def fletch[X, Y](arrow: Δ.ARROW[X, Y]) =
        arrow.asInstanceOf[Δ.ARROW[Any, Any]]

      // wrapping API: TODO make this comment part of the structure

      override def makeDot[S](predot: Δ.DOT[S]): DOT[Δ.WrappedArrow[S]] =
        memoizedWrappedDot(predot)

      override def makeArrow[S, T](prearrow: Δ.ARROW[S, T]): Δ.WrappedArrow[S] > Δ.WrappedArrow[T] =
        AdapterArrow.fromArrow(makeDot(prearrow.source), makeDot(prearrow.target), prearrow)

      override def functionAsArrow[S, T](
        source: DOT[Δ.WrappedArrow[S]],
        target: DOT[Δ.WrappedArrow[T]],
        f: S => T
      ) = makeArrow(Δ.buildArrow[S, T](
        source.dot.asInstanceOf[Δ.DOT[S]],
        target.dot.asInstanceOf[Δ.DOT[T]],
        f
      ).asInstanceOf[Δ.ARROW[Δ.WrappedArrow[S], Δ.WrappedArrow[T]]]
      ).asInstanceOf[Δ.WrappedArrow[S] > Δ.WrappedArrow[T]]

      override def bifunctionAsBiArrow[L, R, T](
        left: DOT[Δ.WrappedArrow[L]],
        right: DOT[Δ.WrappedArrow[R]],
        target: DOT[Δ.WrappedArrow[T]]
      ) (
        bifunc: (L, R) => T
      ) = {
        val targetProduct = makeDot[(L, R)](
          left.dot.asInstanceOf[Δ.DOT[L]] x
            right.dot.asInstanceOf[Δ.DOT[R]]
        )
        BiArrow(left x right, functionAsArrow[(L, R), T](targetProduct, target, {
          case (l, r) => bifunc(l, r)
        }) o
          AdapterArrow.fromArrow(left x right, targetProduct, targetProduct.dot.identity)
        )
      }

      private def biArrow[
        L <: Δ.Element,
        R <: Δ.Element,
        T <: Δ.Element
      ] (
        biArrow: BiArrow[L, R, T]
      ) =
        Δ.BiArrow(
          biArrow.product.left.dot,
          biArrow.product.right.dot,
          biArrow.arrow.arrow.asInstanceOf[Δ.ARROW[(Any, Any), Any]]
        )
    }
    new ElementalToposLayer
  }
}
