package com.fdilke.bewl.topos.algebra

import com.fdilke.bewl.topos.constructions.ConstructToposOfActions
import com.fdilke.bewl.topos.{BaseTopos, LogicalOperations}

trait AlgebraicStructures extends BaseTopos with LogicalOperations with AlgebraicMachinery { builder =>

  import NamedLaws._
  import StandardTermsAndOperators._

  lazy val monoids = AlgebraicTheory(ι, *)(
    "left unit" law ( ι * α := α ),
    "right unit" law ( α * ι := α ),
    "associative" law ( (α * β) * γ := α * (β * γ ) )
  )

  trait CommutativityCriterion { algebra: Algebra =>
    def isCommutative = satisfies(
      α * β := β * α
    )
  }

  case class Monoid[M <: ~](
    override val carrier: DOT[M],
    unit: NullaryOp[M],
    multiply: BinaryOp[M]
  ) extends monoids.Algebra[M](carrier)(
    ι := unit,
    * := multiply
  ) with CommutativityCriterion {
    lazy val actions =
      AlgebraicTheoryWithScalars(carrier)(II := unit)(II, **, ***)(
        "right unit" law ( α ** II := α ),
        "mixed associative" law ( (α ** Φ) ** Ψ := α ** (Φ *** Ψ) )
      )

    def action[A <: ~](actionCarrier: DOT[A])(actionMultiply: (A, M) => A) =
      Action[A](actionCarrier, actionMultiply)

    def regularAction =
      action(carrier) { multiply(_, _) }

    case class Action[A <: ~](actionCarrier: DOT[A], actionMultiply: (A, M) => A) extends
      actions.Algebra[A](actionCarrier)(
        ** := (actionCarrier x carrier).biArrow(actionCarrier)(actionMultiply),
        *** := multiply
      )

    // TODO: can move this class into the topos of actions?
    case class ActionPreArrow[
      S <: ~,
      T <: ~
    ] (
      source: Action[S],
      target: Action[T],
      function: S => T
    )
  }

  lazy val groups = AlgebraicTheory(ι, $minus, *)( // TODO try ~
    "left unit" law ( ι * α := α ),
    "right unit" law ( α * ι := α ),
    "left inverse" law ( (-α) * α := ι ),
    "associative" law ( (α * β) * γ := α * (β * γ ) )
  )

  case class Group[G <: ~](
    override val carrier: DOT[G],
    unit: NullaryOp[G],
    multiply: BinaryOp[G],
    inverse: UnaryOp[G]
  ) extends groups.Algebra[G](carrier)(
    ι := unit,
    * := multiply,
    $minus := inverse
  ) with CommutativityCriterion {
    lazy val asMonoid = Monoid(carrier, unit, multiply)
  }
}