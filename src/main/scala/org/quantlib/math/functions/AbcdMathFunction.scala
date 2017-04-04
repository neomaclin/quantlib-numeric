package org.quantlib.math.functions

import org.quantlib.math.Comparing._
import org.quantlib.math.Constants

abstract class AbcdMathFunction(a: Double = 0.002,
                                b: Double = 0.001,
                                c: Double = 0.16,
                                d: Double = 0.0005) extends (Double => Double) {
  require(c > 0, s"c ($c) must be positive")
  require(d >= 0, s"d ($d) must be non negative")
  require(a + d >= 0, s"a+d ($a+$d) must be non negative")
  require(b >= 0.0)

  val zeroFirstDerivative = 1.0 / c - a / b
  if (zeroFirstDerivative >= 0.0)
    require(
      b >= -(d * c) / Math.exp(c * a / b - 1.0),
      s"b ($b) less than ${-(d * c) / Math.exp(c * a / b - 1.0)}: negative function value at stationary point $zeroFirstDerivative"
    )

  val da = b - c * a
  val db = -c * b
  val dabcd = List(da, db, c, 0.0)
  val abcd = List(a, b, c, d)
  val pa = -(a + b / c) / c
  val pb = -b / c
  val K = 0.0

  val dibc = b / c
  val diacplusbcc = a / c + dibc / c

  // function value at time t: \f[ f(t) \f]
  def apply(t: Double): Double = if (t < 0) 0.0 else (a + b * t) * Math.exp(-c * t) + d

  // time at which the function reaches maximum (if any)
  def maximumLocation: Double = {
    if (b == 0.0) {
      if (a >= 0.0) 0.0 else Constants.QL_MAX_REAL
    } else {
      if (zeroFirstDerivative > 0.0) zeroFirstDerivative else 0.0
    }
  }

  //! maximum value of the function
  def maximumValue = if (b == 0.0 || a <= 0.0) d else apply(maximumLocation)

  //! function value at time +inf: \f[ f(\inf) \f]
  def longTermValue = d

  /*! first derivative of the function at time t
      \f[ f'(t) = [ (b-c*a) + (-c*b)*t) ] e^{-c*t} \f] */
  def derivative(t: Double): Double = if (t < 0) 0.0 else (da + db * t) * Math.exp(-c * t)

  /*! indefinite integral of the function at time t
      \f[ \int f(t)dt = [ (-a/c-b/c^2) + (-b/c)*t ] e^{-c*t} + d*t \f] */
  def primitive(t: Double): Double = if (t < 0) 0.0 else (pa + pb * t) * Math.exp(-c * t) + d * t + K

  /*! definite integral of the function between t1 and t2
      \f[ \int{t1}^{t2} f(t)dt \f] */
  def definiteIntegral(t1: Double, t2: Double): Double = primitive(t2) - primitive(t1)

  def coefficients: List[Double] = abcd

  def derivativeCoefficients: List[Double] = dabcd

  // the primitive is not abcd

  /*! coefficients of a AbcdMathFunction defined as definite
      integral on a rolling window of length tau, with tau = t2-t */
  def definiteIntegralCoefficients(t1: Double, t2: Double): List[Double] = {
    val dt = t2 - t1
    val expcdt = Math.exp(-c * dt)
    List(diacplusbcc - (diacplusbcc + dibc * dt) * expcdt, dibc * (1.0 - expcdt), c, d * dt)
  }

  /*! coefficients of a AbcdMathFunction defined as definite
      derivative on a rolling window of length tau, with tau = t2-t */
  def definiteDerivativeCoefficients(t1: Double, t2: Double): List[Double] = {
    val dt = t2 - t1
    val expcdt = Math.exp(-c * dt)
    List((a * c - b + b * c / (1.0 - expcdt) * dt * expcdt) / 1.0 - expcdt, b * c / (1.0 - expcdt), c, d / dt)
  }

}

final case class AbcdFunction(a: Double = -0.06,
                              b: Double = 0.17,
                              c: Double = 0.54,
                              d: Double = 0.17) extends AbcdMathFunction(a, b, c, d) {

  def covariance(t: Double, T: Double, S: Double): Double = apply(T - t) * apply(S - t)

  def covariance(t1: Double, t2: Double, T: Double, S: Double): Double = {
    require(t1 <= t2, s"integrations bounds ($t1,$t2) are in reverse order")
    val cutOff = Math.min(S, T)
    if (t1 >= cutOff) 0.0
    else {
      primitive(Math.min(t2, cutOff), T, S) - primitive(t1, T, S)
    }

  }

  def volatility(tMin: Double, tMax: Double, T: Double): Double = {
    if (tMax == tMin) instantaneousVolatility(tMax, T);
    require(tMax > tMin, "tMax must be > tMin");
    Math.sqrt(variance(tMin, tMax, T) / (tMax - tMin))
  }

  def variance(tMin: Double, tMax: Double, T: Double): Double = covariance(tMin, tMax, T, T)


  // INSTANTANEOUS
  def instantaneousVolatility(u: Double, T: Double): Double = Math.sqrt(instantaneousVariance(u, T))

  def instantaneousVariance(u: Double, T: Double): Double = instantaneousCovariance(u, T, T)

  def instantaneousCovariance(u: Double, T: Double, S: Double): Double = apply(T - u) * apply(S - u)


  // PRIMITIVE
  def primitive(t: Double, T: Double, S: Double): Double = {
    if (T < t || S < t) 0.0
    else {

      if (c =~ 0.0) {
        val v = a + d
        t * (v * v + v * b * S + v * b * T - v * b * t + b * b * S * T - 0.5 * b * b * t * (S + T) + b * b * t * t / 3.0)
      } else {

        val (k1, k2, k3) = (Math.exp(c * t), Math.exp(c * S), Math.exp(c * T))

        (b * b * (-1 - 2 * c * c * S * T - c * (S + T)
          + k1 * k1 * (1 + c * (S + T - 2 * t) + 2 * c * c * (S - t) * (T - t)))
          + 2 * c * c * (2 * d * a * (k2 + k3) * (k1 - 1)
          + a * a * (k1 * k1 - 1) + 2 * c * d * d * k2 * k3 * t)
          + 2 * b * c * (a * (-1 - c * (S + T) + k1 * k1 * (1 + c * (S + T - 2 * t)))
          - 2 * d * (k3 * (1 + c * S) + k2 * (1 + c * T)
          - k1 * k3 * (1 + c * (S - t))
          - k1 * k2 * (1 + c * (T - t))))) / (4 * c * c * c * k2 * k3)
      }
    }
  }
}

final case class AbcdSquared(a: Double,
                             b: Double,
                             c: Double,
                             d: Double,
                             t: Double,
                             s: Double) extends (Double => Double) {

  def apply(x: Double): Double = AbcdFunction(a, b, c, d).covariance(x, t, s)

}