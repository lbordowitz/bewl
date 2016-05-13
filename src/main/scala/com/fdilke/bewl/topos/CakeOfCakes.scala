package com.fdilke.bewl.topos

import com.fdilke.bewl.topos.algebra.{AlgebraicStructures, AlgebraicConstructions, AlgebraicMachinery}
import com.fdilke.bewl.topos.constructions.{ConstructToposOfAutomorphisms, ConstructToposOfGroupActions, ConstructToposOfMonoidActions}
import com.fdilke.bewl.topos.structures.{StrongMonads, Monads}

trait ToposAlgebra extends
  AlgebraicMachinery with
  AlgebraicConstructions with
  AlgebraicStructures

trait ToposConstructions extends
  BaseTopos with
  ConstructToposOfMonoidActions with
  ConstructToposOfGroupActions with
  ConstructToposOfAutomorphisms {

  Ɛ: AlgebraicStructures with
    AlgebraicMachinery with
    LogicalOperations with
    ToposStructures =>
}

trait ToposStructures extends
  Monads with
  StrongMonads {
  Ɛ: BaseTopos =>
}

trait Topos[BASE] extends BaseTopos with
  Monads with
  LogicalOperations with
  ToposAlgebra with
  ToposConstructions {
  override type ~ = BASE
}
