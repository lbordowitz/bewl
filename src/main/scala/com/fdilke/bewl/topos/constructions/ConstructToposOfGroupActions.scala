package com.fdilke.bewl.topos.constructions

import com.fdilke.bewl.helper.Memoize
import com.fdilke.bewl.topos.algebra.{AlgebraicMachinery, AlgebraicStructures}
import com.fdilke.bewl.topos.{Topos, Wrappings, BaseTopos, LogicalOperations}

trait ConstructToposOfGroupActions extends BaseTopos with LogicalOperations {
  Ɛ: AlgebraicStructures with AlgebraicMachinery =>

  object ToposOfGroupActions {
    def of[G <: ~](group: Ɛ.Group[G]) : Topos with Wrappings[
      Ɛ.~,
      ({type λ[X <: Ɛ.~] = group.Action[X]})#λ,
      ({type λ[X <: Ɛ.~, Y <: Ɛ.~] = group.ActionPreArrow[X, Y]})#λ
    ] = {
      import group.{carrier, multiply, unit, Action, ActionPreArrow}
      class TheTopos extends Topos with Wrappings[
        Ɛ.~,
        ({type λ[X <: Ɛ.~] = group.Action[X]})#λ,
        ({type λ[X <: Ɛ.~, Y <: Ɛ.~] = group.ActionPreArrow[X, Y]})#λ
      ] {
        override type ~ = Ɛ.~
        override type DOT[X <: ~] = ActionDot[X]
        override type >[X <: ~, Y <: ~] = ActionArrow[X, Y]
        override type UNIT = Ɛ.UNIT
        override type TRUTH = Ɛ.TRUTH

        override val I: DOT[UNIT] =
          ActionDot(Ɛ.I) { (i, g) => i }

        override lazy val omega: DOT[TRUTH] =
          ActionDot(Ɛ.omega) { (ω, g) => ω }

        override lazy val truth: >[UNIT, TRUTH] =
          ActionArrow(I, omega, Ɛ.truth)

        override type WRAPPER[T <: ~] = T

        class ActionDot[
          S <: ~
        ] (
          val action: group.Action[S]
        ) extends Dot[S] { dot =>

          override val toI: S > UNIT =
            ActionArrow(this, I, action.actionCarrier.toI)

          override lazy val globals: Traversable[UNIT > S] =
            ???

          override def xUncached[T <: ~](that: DOT[T]): BIPRODUCT[S, T] = {
            val productDot = this.action.actionCarrier x that.action.actionCarrier
            new ActionDot[S x T](
              group.action(productDot){
                case ((s, t), g) =>
                  productDot.pair(
                    this.action.actionMultiply(s, g),
                    that.action.actionMultiply(t, g)
                  )
              }
            ) with BiproductDot[S, T, S x T] {
              override val left = dot
              override val right = that
              override def pair(s: S, t: T) =
                productDot.pair(s, t)
            }
          }

          override def `>Uncached`[T <: ~](that: DOT[T]): EXPONENTIAL[S, T] =
            ???

          override def apply[T <: ~](target: DOT[T])(f: S => T) : S > T =
            ActionArrow(
              this,
              target,
              action.actionCarrier(
                target.action.actionCarrier
              ) { f }
            )

          override def sanityTest = {
            action.actionCarrier.sanityTest
            action.sanityTest
          }
        }

        object ActionDot {
          def apply[
            A <: Ɛ.~
          ] (
             actionCarrier: Ɛ.DOT[A]
          ) (
             actionMultiply: (A, G) => A
          ) =
            new ActionDot(
              group.action(actionCarrier)(actionMultiply)
            )
        }

        case class ActionArrow[
          S <: ~,
          T <: ~
        ](
          override val source: ActionDot[S],
          override val target: ActionDot[T],
          arrow: Ɛ.>[S, T]
        ) extends Arrow[S, T] {
          override lazy val chi: T > TRUTH =
            ???
          override def apply(s: S): T =
            arrow(s)
          override def ?=(that: S > T): EQUALIZER[S] =
            ???
          override def o[R <: ~](that: R > S) =
            ActionArrow(
              that.source,
              target,
              arrow o that.arrow
            )

          override def \[U <: ~](monic: U > T) : S > U =
            ???

          override def sanityTest = {
            arrow.sanityTest
            assert(group.actions.isMorphism(source.action, target.action, arrow))
          }
        }

        override def functionAsArrow[
          S <: ~,
          T <: ~
        ](
          source: ActionDot[S],
          target: ActionDot[T],
          f: S => T
        ): S > T =
          source(target) { f }

        override def makeArrow[
          S <: ~,
          T <: ~
        ] (
          prearrow: group.ActionPreArrow[S, T]
        ): ActionArrow[S, T] =
          ???

        private val memoizedDotWrapper =
          Memoize.generic withLowerBound[
            ({
              type λ[T <: Ɛ.~] = group.Action[T]
            })#λ,
            ActionDot,
            Ɛ.~
          ] {
            new ActionDot(_)
          }

        override def makeDot[
          T <: ~
        ] (
          predot: group.Action[T]
        ) =
          memoizedDotWrapper(predot)
//          new ActionDot(predot)

        override def bifunctionAsBiArrow[
          L <: ~,
          R <: ~,
          T <: ~
        ] (
          left: ActionDot[L],
          right: ActionDot[R],
          target: ActionDot[T]
        ) (
          bifunc: (L, R) => T
        ): BiArrow[L, R, T] =
          (left x right).biArrow(
            target
          ) {
            bifunc
          }
      }
      new TheTopos
    }
  }
}
