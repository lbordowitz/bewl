package com.fdilke.bewl.topos.algebra

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import org.scalatest.{Ignore, FunSpec}
import org.scalatest.Matchers._

class MonadConstructionsTest extends FunSpec {
  private val two = dot('x, 'y)

  import FiniteSets._

  describe("The double-exponential monad") {
    it("can be constructed for sets") {
      val monadJoin = omega.doubleExpMonad

      monadJoin(O).free.globals should have size 2
      monadJoin(I).free.globals should have size 4
      monadJoin(omega).free.globals should have size 16

      val atTwo = monadJoin(two)
      val eta: Symbol > ((Symbol → TRUTH) → TRUTH) = atTwo.eta
      for (
        global <- (two > omega).globals ;
        symbol <- Seq('x, 'y)
      ) {
        val f: Symbol → TRUTH = global(())
        eta(symbol)(f) shouldBe f(symbol)
      }

//      println("Testing 1")
//      monadJoin.sanityTestAt(dot(1))
//      println("Testing 1 2")
//      monadJoin.sanityTestAt(dot(1,2))
//      println("Testing 1 2 3")
//      monadJoin.sanityTestAt(dot(1,2,3)) // a bridge too far?
//      println("Testing 1 2 3 ... done")

// check that M[X] is a monad, via multiplication as structure map. Or is that
// too hard to calculate / a trivial consequence of our existing laws
    }
  }
}
