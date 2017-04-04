package org.quantlib.math.distributions

import org.quantlib.math.Constants._
import org.quantlib.math.functions.Beta

/**
  * Created by neo on 06/03/2017.
  */
final case class BinomialDistribution(p: Double, n: Long) extends (Double => Double) {
  private val (logP, logOneMinusP) = if (p == 0.0) {
    (-QL_MAX_REAL, 0.0)
  } else if (p == 1.0) {
    (0.0, -QL_MAX_REAL)
  } else {
    require(p > 0, "negative p not allowed")
    require(p < 1.0, "p>1.0 not allowed")

    (Math.log(p), Math.log(1.0 - p))
  }

  def apply(k: Long): Double = {
    if (k > n) 0.0
    else {
      if (logP == 0.0) {
        if (k == n) 1.0 else 0.0 // p==1.0
      } else if (logOneMinusP == 0.0) {
        if (k == 0) 1.0 else 0.0 // p==0.0
      } else {
        Math.exp(binomialCoefficientLn(n, k) + k * logP + (n - k) * logOneMinusP)
      }
    }
  }

  def apply(k: Double): Double = apply(k.toLong)
}

final case class CumulativeBinomialDistribution(p: Double, n: Long) extends (Double => Double) {
  require(p >= 0, "negative p not allowed")
  require(p <= 1.0, "p>1.0 not allowed")

  def apply(k: Long): Double = if (k >= n) 1.0 else 1.0 - Beta.incompleteBetaFunction(k + 1, n - k, p)

  def apply(k: Double): Double = apply(k.toLong)
}