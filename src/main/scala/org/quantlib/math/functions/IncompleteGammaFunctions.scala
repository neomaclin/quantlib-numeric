package org.quantlib.math.functions

import java.lang.Math.{abs, exp, log}

import org.quantlib.math.Constants.QL_EPSILON
import org.quantlib.math.distributions.GammaFunction

/**
  * Created by neo on 23/03/2017.
  */
object IncompleteGammaFunctions {

  def incompleteGammaFunction(a: Double, x: Double, accuracy: Double = 1.0e-13, maxIteration: Int = 100): Double = {
    require(a > 0.0, "non-positive a is not allowed")
    require(x >= 0.0, "negative x not allowed")

    if (x < (a + 1.0))
      incompleteGammaFunctionSeriesRepr(a, x, accuracy, maxIteration) // Use the series representation
    else
      1.0 - incompleteGammaFunctionContinuedFractionRepr(a, x, accuracy, maxIteration) // Use the continued fraction representation
  }

  private def incompleteGammaFunctionSeriesRepr(a: Double, x: Double, accuracy: Double = 1.0e-13, maxIteration: Int = 100) = {
    if (x == 0.0) {
      0.0
    } else {
      val gln = GammaFunction.logValue(a)
      var ap = a
      var del, sum = 1.0 / a
      var n = 0
      var result = Double.NaN

      while (n < maxIteration) {
        ap += 1
        del *= x / ap
        sum += del
        if (del.abs < sum.abs * accuracy) {
          result = sum * exp(-x + a * log(x) - gln)
          n = maxIteration
        }
        n += 1
      }

      result
    } ensuring ( _ != Double.NaN, "accuracy not reached")
  }

  private def incompleteGammaFunctionContinuedFractionRepr(a: Double, x: Double, accuracy: Double = 1.0e-13, maxIteration: Int = 100): Double = {
    val gln = GammaFunction.logValue(a)
    var b = x + 1.0 - a
    var c = 1.0 / QL_EPSILON
    var d = 1.0 / b
    var h = d
    var n = 0
    var result = Double.NaN
    while (n < maxIteration) {
      n += 1
      val an = -n * (n - a)
      b += 2.0
      d = an * d + b
      if (scala.math.abs(d) < QL_EPSILON) d = QL_EPSILON
      c = b + an / c
      if (scala.math.abs(c) < QL_EPSILON) c = QL_EPSILON
      d = 1.0 / d
      val del = d * c
      h *= del
      if (abs(del - 1.0) < accuracy) {
        result = exp(-x + a * log(x) - gln) * h
        n = maxIteration + 1
      }
    }
    if (result == Double.NaN) throw new ArithmeticException("accuracy not reached")
    else result
  }

  def betaFunction(z: Double, w: Double): Double = {
    Math.exp(GammaFunction.logValue(z) + GammaFunction.logValue(w) - GammaFunction.logValue(z + w))
  }


}
