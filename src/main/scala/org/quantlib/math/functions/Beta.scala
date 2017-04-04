package org.quantlib.math.functions

import org.quantlib.math.distributions.GammaFunction

import scala.annotation.tailrec

/**
  * Created by neo on 23/03/2017.
  */
object Beta {
  def betaFunction(z:Double, w: Double):Double =
    Math.exp(GammaFunction.logValue(z) +
      GammaFunction.logValue(w) -
      GammaFunction.logValue(z+w))


  def incompleteBetaFunction(a: Double, b: Double, x: Double, accuracy: Double = 1e-16,
                             maxIteration: Int = 100): Double = {

    require(a > 0.0, "a must be greater than zero")
    require(b > 0.0, "b must be greater than zero")


    if (x == 0.0) 0.0
    else if (x == 1.0) 1.0
    else {
      require(x > 0.0 && x < 1.0, "x must be in [0,1]")

      val result = Math.exp(GammaFunction.logValue(a + b) -
        GammaFunction.logValue(a) - GammaFunction.logValue(b) +
        a * Math.log(x) + b * Math.log(1.0 - x))

      if (x < (a + 1.0) / (a + b + 2.0)) result * betaContinuedFraction(a, b, x, accuracy, maxIteration) / a
      else 1.0 - result * betaContinuedFraction(b, a, 1.0 - x, accuracy, maxIteration) / b
    }
  }

  import org.quantlib.math.Constants._
  def betaContinuedFraction(a: Double, b: Double, x: Double, accuracy: Double = 1e-16,
                            maxIteration: Int  = 100): Double = {

    val qab = a + b
    val qap = a + 1.0
    val qam = a - 1.0
    val c = 1.0
    val d = 1.0 / (if (Math.abs(1.0 - qab * x / qap) < QL_EPSILON) QL_EPSILON else 1.0 - qab * x / qap)

    @tailrec
    def buildResult(m: Int, last_c: Double, last_d: Double, last_result: Double): Double = {
      require(m <= maxIteration, "a or b too big, or maxIteration too small in betacf")
      val m2 = 2 * m
      var aa = m * (b - m) * x / ((qam + m2) * (a + m2))
      var d = 1.0 / (if (Math.abs(1.0 + aa * last_d) < QL_EPSILON) QL_EPSILON else 1.0 + aa * last_d)
      var c = if (Math.abs(1.0 + aa / last_c) < QL_EPSILON) QL_EPSILON else 1.0 + aa / last_c

      var result = last_result * d * c
      aa = -(a + m) * (qab + m) * x / ((a + m2) * (qap + m2))
      d = 1.0 / (if (Math.abs(1.0 + aa * d) < QL_EPSILON) QL_EPSILON else 1.0 + aa * d)
      c = if (Math.abs(1.0 + aa / c) < QL_EPSILON) QL_EPSILON else 1.0 + aa / c
      val del = d * c
      result = result * del

      if (Math.abs(del - 1.0) < accuracy) result else buildResult(m + 1, c, d, result)
    }

    buildResult(1, c, d, d)

  }
}
