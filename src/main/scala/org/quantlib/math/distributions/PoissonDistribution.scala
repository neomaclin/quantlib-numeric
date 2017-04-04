package org.quantlib.math.distributions

import org.quantlib.math.functions.IncompleteGammaFunctions._

import java.lang.Math._

import org.quantlib.math.functions.Factorial

sealed abstract class AbstractPoissonDistribution(mu: Double) extends (Double => Double){
  require(mu >= 0.0, s"mu must be non negative ($mu not allowed)")
}

final case class PoissonDistribution(mu: Double) extends AbstractPoissonDistribution(mu){

  def apply(k: Double): Double = {
    if (mu == 0.0) {
      if (k == 0.0) 1.0 else 0.0
    } else {
      val logFactorial = Factorial.ln(k.toInt)
      Math.exp(k*Math.log(mu) - logFactorial - mu)
    }
  }
}

final case class CumulativePoissonDistribution(mu: Double) extends AbstractPoissonDistribution(mu){

  def apply(k: Double): Double = 1.0 - incompleteGammaFunction(k+1, mu)
}

final case class InverseCumulativePoisson(lambda: Double = 1.0) extends (Double => Double){
  require(lambda > 0.0, "lambda must be positive")

  private def calcSummand(index: Long) = exp(-lambda) * pow(lambda, index.toInt) / Factorial.get(index.toInt)

  def apply(x: Double): Double = {
    require(x >= 0.0 && x <= 1.0, "Inverse cumulative Poisson distribution is only defined on the interval [0,1]")

    if (x == 1.0) {
      Double.MaxValue
    } else {
      var sum = 0.0
      var index = 0L
      while (x > sum) {
        sum = sum + calcSummand(index)
        index = index + 1
      }

      (index-1).toDouble
    }
  }
}