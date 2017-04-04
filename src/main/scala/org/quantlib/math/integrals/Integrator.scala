package org.quantlib.math.integrals

import org.quantlib.math.Comparing._

import scala.annotation.tailrec

/**
  * Created by neo on 27/02/2017.
  */
trait Integrator extends (((Double => Double), Double, Double) => Double) {

  def integrate(f: Double => Double, a: Double, b: Double): Double

  def apply(f: Double => Double, a: Double, b: Double): Double = {
    if (a == b) 0.0
    else if (b > a) integrate(f, a, b)
    else -integrate(f, b, a)
  }

}

object Integrator {


  final case class SegmentIntegral(intervals: Int = 10) extends Integrator {
    require(intervals > 0, "at least 1 interval needed, 0 given")

    def integrate(f: Double => Double, a: Double, b: Double): Double = {
      if (a ~= b) 0.0
      else {
        val dx: Double = (b - a) / intervals
        val end = b - 0.5 * dx

        @tailrec
        def sum(x: Double, result: Double): Double = {
          if (x >= end) result else sum(x + dx, result + f(x))
        }

        sum(a + dx, 0.5 * (f(a) + f(b))) * dx

      }
    }
  }


  final case class TwoDimensionalIntegral(xi: Integrator, yi: Integrator) {

    def apply(f: (Double, Double) => Double,
              a: (Double, Double),
              b: (Double, Double)): Double = xi(x => yi(f.curried(x), a._2, b._2), a._1, b._1)


  }


}






