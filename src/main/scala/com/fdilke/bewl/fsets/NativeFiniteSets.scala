package com.fdilke.bewl.fsets

import com.fdilke.bewl.fsets.DiagrammaticFiniteSetsUtilities._
import com.fdilke.bewl.helper.Memoize
import com.fdilke.bewl.topos.{Wrappings, Topos}

object NativeFiniteSets extends Topos with Wrappings[Traversable, FiniteSetsPreQuiver] {
  override type ELEMENT = Any
  override type STAR[S <: ELEMENT] = FiniteSetsStar[S]
  override type QUIVER[S <: ELEMENT, T <: ELEMENT] = FiniteSetsQuiver[S, T]
  override type UNIT = Unit
  override type TRUTH = Boolean
  override lazy val I = star(Traversable(()))
  override lazy val omega = star(Traversable(true, false))
  override lazy val truth = I(omega) { _ => true }

  class FiniteSetsStar[S](elements: Traversable[S])
    extends Star[S] with Traversable[S] { self =>
    override lazy val toI = this(I) { _ => () }

    private val memoizedExponential = {
      type CURRIED_EXPONENTIAL[T <: ELEMENT] = EXPONENTIAL[S, T]
      def exponential[T <: ELEMENT](that: STAR[T]) = {
        case class FunctionElement(function: S => T) extends (S => T) {
          override def equals(that: scala.Any): Boolean = that match {
              case that: FunctionElement => elements.forall {
                s => function(s) == that.function(s)
              }}
          override def hashCode = 0
          def apply(s: S): T = function(s)
        }
        new FiniteSetsStar[S > T](
          allMaps(self, that).map { FunctionElement } // TODO: coalesce
        ) with ExponentialStar[S, T] { exponentialStar =>
          override val source: STAR[S] = self
          override val target: STAR[T] = that

          override def transpose[R <: ELEMENT](biQuiver: BiQuiver[R, S, T]) =
            biQuiver.product.left(exponentialStar) {
              r => FunctionElement {
              s => biQuiver(r, s)
            }}}}
      Memoize.generic.withLowerBound[STAR, CURRIED_EXPONENTIAL, ELEMENT](exponential)
    }
    override def >[T <: ELEMENT](that: STAR[T]): EXPONENTIAL[S, T] = memoizedExponential(that)

    private val memoizedProduct = {
      type CURRIED_BIPRODUCT[T <: ELEMENT] = BIPRODUCT[S, T]
      def product[T <: ELEMENT](that: STAR[T]) =
        new FiniteSetsStar[S x T](
          for(s <- this ; t <- that)
            yield (s, t)
        ) with BiproductStar[S, T] {
          override val left: STAR[S] = self
          override val right: STAR[T] = that
          override def pair(l: S, r: T): x[S, T] = (l, r)
        }
      Memoize.generic.withLowerBound[STAR, CURRIED_BIPRODUCT, ELEMENT](product)
    }
    override def x[T <: ELEMENT](that: STAR[T]): BIPRODUCT[S, T] = memoizedProduct(that)

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
      new FiniteSetsStar[S] (
        source.filter { s => function(s) == that.function(s) }
      ) with EqualizingStar[S] { equalizer =>
        override val equalizerTarget = source
        override def restrict[R](substar: QUIVER[R, S]) =
          substar.source(this) { substar(_) }
        override val inclusion: QUIVER[S, S] = equalizer(source) { s => s }
      }

    override def apply(s: S) =
      function(s)

    override def o[R <: ELEMENT](that: QUIVER[R, S]) =
      that.source(target)(function compose that.function)

    override lazy val chi =
      target(omega) { t =>
        source.exists { s =>
          this(s) == t
      }}

    override def equals(other: Any): Boolean = other match {
      case that: FiniteSetsQuiver[S, T] =>
        source == that.source && target == that.target &&
          source.forall(x => function(x) == that.function(x))
      case _ => false
    }
    override def hashCode = 0

    override def toString =
      s"FiniteSetsQuiver[$source -> $target : ${
        for(s <- source)
          s -> this(s)
      }]"
  }

  private val memoizedStarWrapper = {
    def wrap[T](elements: Traversable[T]) =
      new FiniteSetsStar(elements)
    Memoize.generic(wrap)
  }

  // wrapping API: TODO build this comment into the structure

  override type WRAPPER[T] = T

  override def functionAsQuiver[S, T](source: STAR[S], target: STAR[T], f: S => T) =
    source(target)(f)

  override def quiver[S, T](prequiver: FiniteSetsPreQuiver[S, T]) =
    star(prequiver.source)(star(prequiver.target))(prequiver.function)

  override def star[T](input: Traversable[T]) =
    memoizedStarWrapper(input)

  // unusually simple generic definition for this topos because WRAPPER is trivial
  override def bifunctionAsBiQuiver[L, R, T](
    left: STAR[L],
    right: STAR[R],
    target: STAR[T]
  ) (
    bifunc: (L, R) => T
  ): BiQuiver[L, R, T] =
    (left x right).biQuiver(target) { bifunc }
}

case class FiniteSetsPreQuiver[S, T](
  source: Traversable[S],
  target: Traversable[T],
  function: S => T
)

object NativeFiniteSetsUtilities {
  import NativeFiniteSets._

  def makeStar[T](elements: T*) = star(elements)

  def makeQuiver[S, T](source: STAR[S], target: STAR[T], map: (S, T)*) =
    functionAsQuiver(source, target, Map(map: _*))

  def makeBiQuiver[L, R, T](
                             left: STAR[L],
                             right: STAR[R],
                             target: STAR[T],
                             mappings: ((L, R), T)*
                             ) =
    bifunctionAsBiQuiver[L, R, T](left, right, target) { (l, r) => Map(mappings:_*)((l, r)) }

  def makeNullaryOperator[X](carrier: STAR[X], value: X) =
    functionAsQuiver(I, carrier, (_: UNIT) => value)

  def makeBinaryOperator[X](
    carrier: STAR[X],
    mappings: ((X, X), X)*
  ) =
    bifunctionAsBiQuiver[X](carrier) { (x, y) => Map(mappings:_*)((x, y)) }
}