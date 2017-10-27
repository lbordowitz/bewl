package com.fdilke.bewl.helper

import scala.language.higherKinds
import scala.collection.JavaConverters._
import scala.collection.mutable

object Memoize {

  private def concurrentMap[
    A, B
  ]: mutable.Map[A, B] =
    new java.util.concurrent.ConcurrentHashMap[
      A, B
    ].asScala

  def apply[INPUT, OUTPUT](
    function: INPUT => OUTPUT
  ) =
    new MemoizedFunction[INPUT, OUTPUT](
      function
    )

  class MemoizedFunction[INPUT, OUTPUT](
      function: INPUT => OUTPUT
  ) {
    private val resultMap =
      concurrentMap[INPUT, OUTPUT]

    def apply(input: INPUT): OUTPUT =
      resultMap.getOrElseUpdate(
        input, 
        function(input)
      )
  }

  object generic {
    def apply[INPUT[T], OUTPUT[T]](
      function: INPUT[Nothing] => OUTPUT[Nothing]
    ) =
      new MemoizedFunctionGeneric[INPUT, OUTPUT, Any](
        function.asInstanceOf[
          INPUT[_] => OUTPUT[_]
        ]
      )

    class MemoizedFunctionGeneric[
      INPUT[T <: BASE], 
      OUTPUT[T <: BASE], 
      BASE
    ](
        function: INPUT[_ <: BASE] => OUTPUT[_ <: BASE]
    ) {
      private val resultMap = 
        concurrentMap[
          INPUT[_], 
          OUTPUT[_]
        ]

      def apply[T <: BASE](input: INPUT[T]): OUTPUT[T] =
        resultMap.getOrElseUpdate(
            input, 
            function(input)
        ).asInstanceOf[OUTPUT[T]]
    }

    def withLowerBound[
      INPUT[T <: BASE],
      OUTPUT[T <: BASE],
      BASE
    ](
      function: INPUT[BASE] => OUTPUT[BASE]
    ) =
      new MemoizedFunctionGeneric[INPUT, OUTPUT, BASE](
        function.asInstanceOf[
          INPUT[_ <: BASE] => OUTPUT[_ <: BASE]
        ]
      )
  }
}


