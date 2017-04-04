package org.quantlib.math


import org.quantlib.math.Comparison.{Proximity, _}

object Comparing {

    implicit class ProximityOpsClass[T](val self: T)  extends AnyVal {
       def ~=(other: T)(implicit size: Int = 42, ev: Proximity[T]): Boolean = ev.~=(self, other)(size)
       def =~(other: T)(implicit size: Int = 42, ev: Proximity[T]): Boolean = ev.=~(self, other)(size)
    }

    implicit object DoubleProximity extends Proximity[Double] {
      def ~=(x:Double ,y: Double)(implicit size: Int): Boolean = {
        if (x == y) {
          true
        } else {
          val diff = Math.abs( x - y )
          val tolerance = size * Constants.QL_EPSILON
          if (x * y == 0.0) // x or y = 0.0
            diff < (tolerance * tolerance)
          else
            diff <= tolerance * Math.abs(x) || diff <= tolerance * Math.abs(y)
        }
      }
      def =~(x:Double ,y: Double)(implicit size: Int): Boolean = {
        if (x == y) {
          true
        } else {
          val diff = Math.abs( x - y )
          val tolerance = size * Constants.QL_EPSILON
          if (x * y == 0.0) // x or y = 0.0
            diff < (tolerance * tolerance)
          else
            diff <= tolerance * Math.abs(x) && diff <= tolerance * Math.abs(y)
        }
      }
    }


}