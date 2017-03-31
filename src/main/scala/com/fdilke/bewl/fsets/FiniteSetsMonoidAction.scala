package com.fdilke.bewl.fsets

import FiniteSetsUtilities._
import FiniteSets.{~, >}
import scala.language.higherKinds
import scala.language.postfixOps

trait AbstractCyclic[A] {
  val generator: A
}

trait AbstractCyclics[A] {
  val cyclics: Seq[AbstractCyclic[A]]    
  val transversal: Seq[A]

  def contains(a: A): Boolean
  def +(a: A): AbstractCyclics[A]
  def <<(a: A): AbstractCyclics[A]
}

trait AbstractActionAnalysis[A] {
  val initialCyclics: AbstractCyclics[A]
  val generators: Seq[A]
}

object FiniteSetsMonoidAction {
  def apply[M](
    monoid: FiniteSets.Monoid[M]
  ) = {
    val monoidElements = 
      elementsOf(monoid.carrier)
      
    new monoid.ActionAnalyzer[
      ({type λ[X <: ~] = AbstractActionAnalysis[X] with 
        monoid.MorphismEnumerator[X]}) # λ
    ] {
      override def analyze[A](
        action: monoid.Action[A]
      ) = new AbstractActionAnalysis[A] with 
        action.Analysis with 
        monoid.MorphismEnumerator[A] {

        case class Cyclic(
           override val generator: A
        ) extends AbstractCyclic[A] {
         val elements: Set[A] =
            monoidElements map { (m : M) =>
              action.actionMultiply(
                generator, 
                m
              )
            } toSet
          
          def contains(a: A) =
            elements contains a
      
          def subsetOf(
            other: Cyclic
          ) =
            elements.subsetOf(
              other.elements          
            )
        }
        
        class MaximalCyclics(
          override val cyclics: Seq[Cyclic] =
            Seq.empty
        ) extends AbstractCyclics[A] { self =>
          
          override def contains(a: A) =
            cyclics.exists { 
              _ contains a 
            }
          
          private def +(
            newCyclic: Cyclic
          ) = 
            new MaximalCyclics(
              newCyclic +: ( 
                cyclics filterNot { 
                  _ subsetOf newCyclic
                } 
              )
            )
      
          override def +(
            a: A
          ): MaximalCyclics = 
            self + Cyclic(a)
            
          override lazy val transversal =
            cyclics map {
              _.generator
            }
          
          override def <<(a: A): MaximalCyclics = 
            if (self contains a)
              self
            else 
              self + Cyclic(a)
        }
        
        private val actionElements = 
          elementsOf(action.carrier)
        
        override lazy val initialCyclics =
          new MaximalCyclics
        
        override lazy val generators = 
          actionElements.foldLeft(
            initialCyclics
          ) { 
            _ << _
          }.transversal
          
        override def morphismsTo[B <: ~](
          target: monoid.Action[B] 
        ) =
          new Traversable[A > B] {
            override def foreach[U](
              f: (A > B) => U
            ) {
              ???
            }
          }
      }
    }
  }
}
