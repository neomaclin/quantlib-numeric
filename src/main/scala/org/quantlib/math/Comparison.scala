package org.quantlib.math

object Comparison {

  type Operator = (Double, Double) => Boolean

  trait Order[A]{
    def >(x: A, y: A): Boolean
    def <(x: A, y: A): Boolean
    def >=(x: A, y: A): Boolean = ! <(x, y)
    def <=(x: A, y: A): Boolean = ! >(x, y)
  }

  trait Proximity[A]{
    def ~=(x: A, y: A)(implicit size: Int = 42): Boolean
    def =~(x: A, y: A)(implicit size: Int = 42): Boolean
  }

}
