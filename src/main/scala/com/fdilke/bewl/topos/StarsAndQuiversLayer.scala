package com.fdilke.bewl.topos

import com.fdilke.bewl.diagrammatic.BaseDiagrammaticTopos
import com.fdilke.bewl.helper.Memoize

import scala.Function._

trait BetterWrappings[PRESTAR[_], PREQUIVER[_, _]] { topos: BaseTopos =>
  type WRAPPER[T] <: ELEMENT

  def star[T](input: PRESTAR[T]) : STAR[WRAPPER[T]]
  def quiver[S, T](connector: PREQUIVER[S, T]) : QUIVER[WRAPPER[S], WRAPPER[T]]
  def functionAsQuiver[S, T](source: STAR[WRAPPER[S]], target: STAR[WRAPPER[T]], f: S => T): QUIVER[WRAPPER[S], WRAPPER[T]]
  def bifunctionAsBiQuiver[L, R, T] (
                                      left: STAR[WRAPPER[L]],
                                      right: STAR[WRAPPER[R]],
                                      target: STAR[WRAPPER[T]],
                                      bifunc: (L, R) => T
                                      ): BiQuiver[WRAPPER[L], WRAPPER[R], WRAPPER[T]]
}

trait StarsAndQuiversLayer extends BaseDiagrammaticTopos {

  lazy val adapterLayer : BaseTopos with BetterWrappings[DOT, ARROW] = new FancyTopos

  private class FancyTopos extends BaseTopos with BetterWrappings[DOT, ARROW] {

    override type ELEMENT = Element

    trait Element {
      protected[StarsAndQuiversLayer] val arrow: ARROW[Any, Any]
    }

    override type STAR[S <: ELEMENT] = AdapterStar[S]
    override type QUIVER[S <: ELEMENT, T <: ELEMENT] = AdapterQuiver[S, T]

    override type UNIT = WrappedArrow[Unit]
    lazy val I: STAR[UNIT] = star(StarsAndQuiversLayer.this.I).asInstanceOf[STAR[UNIT]]

    override type TRUTH = AdapterTruth
    override lazy val omega = star(StarsAndQuiversLayer.this.omega).asInstanceOf[STAR[TRUTH]]
    override lazy val truth = quiver(StarsAndQuiversLayer.this.truth).asInstanceOf[QUIVER[UNIT, TRUTH]]

    trait AdapterTruth extends Element

    trait AdapterStar[T <: Element] extends Star[T] {
      self =>
      override lazy val toI: QUIVER[T, UNIT] =
        quiver(dot.toI).asInstanceOf[QUIVER[T, UNIT]]

      private val memoizedProductStar = {
        type CURRIED_BIPRODUCT[U <: ELEMENT] = BIPRODUCT[T, U]
        def product[U <: ELEMENT](that: STAR[U]) =
          new AdapterStar[T x U] with BiproductStar[T, U] {
            override val left = self
            override val right = that

            override def pair(t: T, u: U) = asElement(t.arrow x u.arrow)

            override private[StarsAndQuiversLayer] val dot = (self.dot x that.dot).asInstanceOf[DOT[Any]]

            override private[StarsAndQuiversLayer] def asElement(anArrow: ARROW[_, _]) =
              new (T, U)(
                self.asElement(fletch(StarsAndQuiversLayer.this.leftProjection(self.dot, that.dot))(fletch(anArrow))),
                that.asElement(fletch(StarsAndQuiversLayer.this.rightProjection(self.dot, that.dot))(fletch(anArrow)))
              ) with Element {
                override val arrow: ARROW[Any, Any] = fletch(anArrow)
              }
          }
        Memoize.generic.withLowerBound[STAR, CURRIED_BIPRODUCT, ELEMENT](product)
      }

      override def x[U <: Element](that: STAR[U]): BIPRODUCT[T, U] =
        memoizedProductStar(that)

      private val memoizedExponentialStar = {
        type CURRIED_EXPONENTIAL[U <: ELEMENT] = EXPONENTIAL[T, U]
        def exponential[U <: ELEMENT](that: STAR[U]) =
          new AdapterStar[T > U] with ExponentialStar[T, U] {
            override val source = AdapterStar.this
            override val target = that

            private val exponential = target.dot A source.dot
            override private[StarsAndQuiversLayer] val dot = exponential.exponentDot.asInstanceOf[DOT[Any]]

            override def transpose[R <: ELEMENT](biQuiver: BiQuiver[R, T, U]) =
              biQuiver.product.left(this, exponential.transpose(biArrow(biQuiver)))

            override private[StarsAndQuiversLayer] def asElement(anArrow: ARROW[_, _]) =
              new (T => U) with Element {
                override val arrow: ARROW[Any, Any] = fletch(anArrow)

                override def apply(s: T): U =
                  target.asElement(
                    StarsAndQuiversLayer.this.evaluation(source.dot, target.dot)(
                      anArrow.asInstanceOf[ARROW[Any, Any => Any]],
                      s.arrow
                    ))
              }
          }
        Memoize.generic.withLowerBound[STAR, CURRIED_EXPONENTIAL, ELEMENT](exponential)
      }

      override def >[U <: ELEMENT](that: STAR[U]) =
        memoizedExponentialStar(that)

      override def sanityTest = dot.sanityTest

      override def apply[U <: ELEMENT](target: STAR[U])(f: T => U) =
        AdapterQuiver[T, U](this, target, f)

      private[StarsAndQuiversLayer] val dot: DOT[Any]

      private[StarsAndQuiversLayer] def apply[U <: Element](
                                                               target: STAR[U],
                                                               arrow: ARROW[_, _]
                                                               ): QUIVER[T, U] = this(target)(
        t => target.asElement(fletch(arrow)(t.arrow))
      )

      private[StarsAndQuiversLayer] def asElement(arrow: ARROW[_, _]): T
    }

    case class AdapterQuiver[S <: Element, T <: Element](
                                                          source: STAR[S],
                                                          target: STAR[T],
                                                          function: S => T
                                                          ) extends Quiver[S, T] {
      override def apply(s: S) = function(s)

      override def o[R <: ELEMENT](that: QUIVER[R, S]) =
        that.source(target)(function compose that.function)

      def ?=(that: QUIVER[S, T]) =
        new AdapterStar[S] with EqualizingStar[S] {
          private val equalizer = arrow ?= that.arrow
          override val equalizerTarget = AdapterQuiver.this.source

          override private[StarsAndQuiversLayer] val dot = equalizer.equalizerSource.asInstanceOf[DOT[Any]]

          override private[StarsAndQuiversLayer] def asElement(anArrow: ARROW[_, _]): S =
            equalizerTarget.asElement(fletch(equalizer.equalizer)(fletch(anArrow)))

          override def restrict[R <: ELEMENT](quiver: QUIVER[R, S]) =
            quiver.source(this, equalizer.restrict(quiver.arrow))

          val inclusion: QUIVER[S, S] =
            this(source) { s => s}
        }

      override def equals(other: Any) = other match {
        case that: QUIVER[S, T] => arrow == that.arrow
        case _ => false
      }

      override lazy val chi = target(omega, arrowChi.arrow)

      override def \[U <: ELEMENT](monic: QUIVER[U, T]) =
        source(monic.source, monic.arrowChi.restrict(arrow))

      override def sanityTest = arrow.sanityTest

      private[StarsAndQuiversLayer] lazy val arrow =
        function(source.asElement(source.dot.identity)).arrow

      private lazy val arrowChi = arrow.chi
    }

    class WrappedArrow[X](protected[StarsAndQuiversLayer] val arrow: ARROW[Any, Any]) extends ELEMENT {
      override def toString: String = s"WrappedArrow($arrow)"
    }

    private class WrappedDot[X](innerDot: DOT[X]) extends STAR[WrappedArrow[X]] {
      override private[StarsAndQuiversLayer] val dot: DOT[Any] = innerDot.asInstanceOf[DOT[Any]]

      override private[StarsAndQuiversLayer] def asElement(arrow: ARROW[_, _]) =
        new WrappedArrow(fletch(arrow))
    }

    private val memoizedWrappedDot = {
      def wrapDot[T](dot: DOT[T]) = new WrappedDot(dot)
      Memoize.generic(wrapDot)
    }

    private def fletch[X, Y](arrow: ARROW[X, Y]) =
      arrow.asInstanceOf[ARROW[Any, Any]]

    // wrapping API: TODO make this comment part of the structure

    override type WRAPPER[S] = WrappedArrow[S]

    override def star[S](dot: DOT[S]): STAR[WrappedArrow[S]] =
      memoizedWrappedDot(dot)

    override def quiver[S, T](arrow: ARROW[S, T]): QUIVER[WRAPPER[S], WRAPPER[T]] =
      star(arrow.source)(star(arrow.target), arrow)

    override def functionAsQuiver[S, T](
                                         source: STAR[WrappedArrow[S]],
                                         target: STAR[WrappedArrow[T]],
                                         f: S => T
                                         ) = quiver(buildArrow[S, T](
      source.dot.asInstanceOf[DOT[S]],
      target.dot.asInstanceOf[DOT[T]],
      f
    ).asInstanceOf[ARROW[WRAPPER[S], WRAPPER[T]]]
    ).asInstanceOf[QUIVER[WRAPPER[S], WRAPPER[T]]]

    override def bifunctionAsBiQuiver[L, R, T](
                                                left: STAR[WRAPPER[L]],
                                                right: STAR[WRAPPER[R]],
                                                target: STAR[WRAPPER[T]],
                                                bifunc: (L, R) => T
                                                ) = {
      val targetProduct = star[(L, R)](
        left.dot.asInstanceOf[DOT[L]] x
          right.dot.asInstanceOf[DOT[R]]
      )
      BiQuiver(left x right, functionAsQuiver[(L, R), T](targetProduct, target, {
        case (l, r) => bifunc(l, r)
      }) o
        (left x right)(targetProduct, targetProduct.dot.identity)
      )
    }

    private def biArrow[L <: ELEMENT, R <: ELEMENT, T <: ELEMENT](biQuiver: BiQuiver[L, R, T]) =
      BiArrow(biQuiver.product.left.dot,
        biQuiver.product.right.dot,
        biQuiver.quiver.arrow.asInstanceOf[ARROW[(Any, Any), Any]])
  }
}