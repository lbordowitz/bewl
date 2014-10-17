package com.fdilke.bewl.fsets

import com.fdilke.bewl.fsets.DiagrammaticFiniteSetsUtilities._
import com.fdilke.bewl.helper.Memoize
import com.fdilke.bewl.topos.{Topos, Wrappings}

class NativeFiniteSets extends Topos
  with Wrappings[Traversable, FiniteSetsPreQuiver] {

  override type ELEMENT = Any
  override type STAR[S <: ELEMENT] = FiniteSetsStar[S]
  override type QUIVER[S <: ELEMENT, T <: ELEMENT] = FiniteSetsQuiver[S, T]
  override type UNIT = Unit
  override type TRUTH = Boolean
  override val I = star(Traversable(()))
  override val omega = star(Traversable(true, false))
  override val truth = I(omega) { _ => true }

  class FiniteSetsStar[S](elements: Traversable[S])
    extends Star[S] with Traversable[S] { self =>
    override lazy val toI = this(I) { _ => () }

    private val memoizedExponential = {
      type EXPONENTIAL[T <: ELEMENT] = ExponentialStar[S, T] with STAR[S > T]
      def exponential[T <: ELEMENT](that: STAR[T]) = {
        case class FunctionElement(function: S => T) extends ~>[S, T] {
        override def equals(that: scala.Any): Boolean = that match {
            case that: FunctionElement => elements.forall {
              s => function(s) == that.function(s)
            }}
          override def hashCode = 0 // consistent semantics; no one will use them as keys
          def apply(s: S): T = function(s) // Could check membership of the domain, but we don't
        }
        new FiniteSetsStar[S > T](
          allMaps(self, that).map { FunctionElement } // TODO: coalesce
        ) with ExponentialStar[S, T] { exponentialStar =>
          override val source: STAR[S] = self
          override val target: STAR[T] = that

          override def transpose[R <: ELEMENT](biQuiver: BiQuiver[R, S, T]) =
            biQuiver.left(exponentialStar) {
              r => FunctionElement {
              s => biQuiver(r, s)
            }}}}
      Memoize.generic.withLowerBound[STAR, EXPONENTIAL, ELEMENT](exponential)
    }
    override def >[T <: ELEMENT](that: STAR[T]): ExponentialStar[S, T] with STAR[S > T] = memoizedExponential(that)

    private val memoizedProduct = {
      type PRODUCT[T <: ELEMENT] = ProductStar[S, T] with STAR[S x T]
      def _pair[T <: ELEMENT](s: S, t: T): S x T =
        new xI[S, T] {
          override val left = s
          override val right = t
        }
      def product[T <: ELEMENT](that: STAR[T]) =
        new FiniteSetsStar[S x T](
        for(s <- this ; t <- that)
        yield _pair(s, t)
        ) with ProductStar[S, T] {
          override val left: STAR[S] = self
          override val right: STAR[T] = that
          override def pair(l: S, r: T): x[S, T] = _pair(l, r)
//
//          override def foreach[U](f: (x[S, T]) => U): Unit = ???
//
//          override def apply[T <: ELEMENT](target: STAR[T])(f: (x[S, T]) => T): QUIVER[x[S, T], T] = ???
        }
      Memoize.generic.withLowerBound[STAR, PRODUCT, ELEMENT](product)
    }
    override def x[T <: ELEMENT](that: STAR[T]): ProductStar[S, T] with STAR[x[S, T]] = memoizedProduct(that)

    override def apply[T <: ELEMENT](target: STAR[T])(f: S => T) =
      new FiniteSetsQuiver(this, target, f)

    override def sanityTest =
      for (x <- elements ; y <- elements)
        x == y

    override def foreach[U](f: S => U) =
      elements foreach f
  }

  class FiniteSetsQuiver[S, T](
    val source: FiniteSetsStar[S],
    val target: FiniteSetsStar[T],
    private[NativeFiniteSets] val function: S => T
  ) extends Quiver[S, T] { self =>
    override def \[U <: ELEMENT](monic: QUIVER[U, T]) =
      source(monic.source) { s =>
        val quarry: T = function(s)
        monic.source.find { u =>
          monic(u) == quarry
        } getOrElse {
          throw new IllegalArgumentException(s"Cannot backdivide $self by monic $monic")
      }}
    override def sanityTest =
      if (!source.map(function).forall(x => target.exists(_ == x))) {
        throw new IllegalArgumentException("Map values not in target")
      }
    override def ?=(that: QUIVER[S, T]) =
      new FiniteSetsStar[EqualizingElement[S] with Any] (
        for (s <- source if function(s) == that.function(s))
          yield new EqualizingElement[S] {
            override val include = s

            override def equals(obj: scala.Any): Boolean = {
              println("Aaargh") // TODO: note: may be an issue about comparing equality with these
              super.equals(obj)
            }
        }
      ) with EqualizingStar[S] {
        override val equalizerTarget = source
        override def restrict[R](substar: QUIVER[R, S]) =
          substar.source(this) { r: R =>
            val quarry = substar(r)
            find {
              _.include == quarry
            } getOrElse {
              throw new IllegalArgumentException(s"Cannot restrict $self by monic $substar") // TODO: need this logic twice??
            }}}

    override def x[U <: ELEMENT](that: QUIVER[S, U]): QUIVER[S, x[T, U]] = ???

    override def apply(s: S) =
      function(s)

    override def o[R <: ELEMENT](that: QUIVER[R, S]) =
      that.source(target)(function compose that.function)

    override lazy val chi =
      target(omega) { t =>
        source.exists { s =>
          this(s) == t
      }}
  }

  private val memoizedStarWrapper = {
    def wrap[T](elements: Traversable[T]) =
      new FiniteSetsStar(elements)
    Memoize.generic(wrap)
  }

  // wrapping API

  override type WRAPPER[T] = T

  override def functionAsQuiver[S, T](source: STAR[S], target: STAR[T], f: S => T) =
    source(target)(f)

  override def quiver[S, T](prequiver: FiniteSetsPreQuiver[S, T]) =
    star(prequiver.source)(star(prequiver.target))(prequiver.function)

  override def star[T](input: Traversable[T]) =
    memoizedStarWrapper(input)

  override def bifunctionAsBiQuiver[L, R, T](left: STAR[L], right: STAR[R], target: STAR[T], bifunc: (L, R) => T): BiQuiver[L, R, T] = ???
}

case class FiniteSetsPreQuiver[S, T](
  source: Traversable[S],
  target: Traversable[T],
  function: S => T
)
