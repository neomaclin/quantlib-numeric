package org.quantlib.math.functions

import org.quantlib.math.Constants.QL_EPSILON
import org.quantlib.math.Comparing._
/**
  * Created by neo on 23/03/2017.
  */
object Sabr {

  def unsafeSabrVolatility(strike: Double,
                           forward: Double,
                           expiryTime: Double,
                           alpha: Double,
                           beta: Double,
                           nu: Double,
                           rho: Double): Double = {
    val oneMinusBeta = 1.0 - beta
    val A = Math.pow(forward * strike, oneMinusBeta)
    val sqrtA = Math.sqrt(A)
    val logM =
      if (!(forward =~ strike)) Math.log(forward / strike)
      else {
        val epsilon = (forward - strike) / strike
        epsilon -.5 * epsilon * epsilon
      }
    val z = (nu / alpha) * sqrtA * logM
    val B = 1.0 - 2.0 * rho * z + z * z
    val C = oneMinusBeta * oneMinusBeta * logM * logM
    val tmp = (Math.sqrt(B) + z - rho) / (1.0 - rho)
    val xx = Math.log(tmp)
    val D = sqrtA * (1.0 + C / 24.0 + C * C / 1920.0)
    val d = 1.0 + expiryTime *
      (oneMinusBeta * oneMinusBeta * alpha * alpha / (24.0 * A) +
        0.25 * rho * beta * nu * alpha / sqrtA +
        (2.0 - 3.0 * rho * rho) * (nu * nu / 24.0))

    // computations become precise enough if the square of z worth
    // slightly more than the precision machine (hence the m)
    val m = 10
    val multiplier = if (Math.abs(z * z) > QL_EPSILON * m) z / xx
    else 1.0 - 0.5 * rho * z - (3.0 * rho * rho - 2.0) * z * z / 12.0

    (alpha / D) * multiplier * d
  }

  def unsafeShiftedSabrVolatility(strike: Double,
                                  forward: Double,
                                  expiryTime: Double,
                                  alpha: Double,
                                  beta: Double,
                                  nu: Double,
                                  rho: Double,
                                  shift: Double): Double = {
    unsafeSabrVolatility(strike + shift, forward, expiryTime, alpha, beta, nu, rho)
  }

  def sabrVolatility(strike: Double,
                     forward: Double,
                     expiryTime: Double,
                     alpha: Double,
                     beta: Double,
                     nu: Double,
                     rho: Double): Double = {
    require(strike > 0.0)
    require(forward > 0.0)
    require(expiryTime >= 0.0)
    require(alpha > 0.0)
    require(beta >= 0.0 && beta <= 1.0)
    require(nu >= 0.0)
    require(rho * rho < 1.0)
    unsafeSabrVolatility(strike, forward, expiryTime, alpha, beta, nu, rho)
  }

  def shiftedSabrVolatility(strike: Double,
                            forward: Double,
                            expiryTime: Double,
                            alpha: Double,
                            beta: Double,
                            nu: Double,
                            rho: Double,
                            shift: Double): Double = {
    require(strike + shift > 0.0)
    require(forward + shift > 0.0)
    require(expiryTime >= 0.0)
    require(alpha > 0.0)
    require(beta >= 0.0 && beta <= 1.0)
    require(nu >= 0.0)
    require(rho * rho < 1.0)
    unsafeSabrVolatility(strike, forward, expiryTime, alpha, beta, nu, rho)
  }
}