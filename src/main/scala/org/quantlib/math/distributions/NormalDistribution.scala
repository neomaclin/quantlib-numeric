package org.quantlib.math.distributions

import org.quantlib.math.Constants._
import org.quantlib.math.Comparing._
import org.quantlib.math.functions.ErrorFunction

sealed abstract class GaussianDistribution(average: Double, sigma: Double) extends (Double => Double) {
  require(sigma > 0.0, s"sigma must be greater than 0.0 ($sigma not allowed)")
}

//Using apache common for the following MaddockCumulativeNormal/InverseCumulativeNormal implementation

final case class MaddockCumulativeNormal(average: Double = 0.0,
                                         sigma: Double = 1.0) extends GaussianDistribution(average, sigma){
  private val impl = new org.apache.commons.math3.distribution.NormalDistribution(average, sigma)
  def apply(x: Double): Double = impl.cumulativeProbability(x)

}

final case class MaddockInverseCumulativeNormal(average: Double = 0.0,
                                                sigma: Double = 1.0) extends GaussianDistribution(average, sigma) {
  private val impl = new org.apache.commons.math3.distribution.NormalDistribution(average, sigma)
  def apply(x: Double): Double = impl.inverseCumulativeProbability(x)
}

final case class NormalDistribution(average: Double = 0.0,
                                    sigma: Double = 1.0) extends GaussianDistribution(average, sigma) {

  private val normalizationFactor = M_SQRT_2 * M_1_SQRTPI / sigma
  private val derNormalizationFactor = sigma * sigma
  private val denominator = 2.0 * derNormalizationFactor

  def apply(x: Double): Double = {
    val deltaX = x - average
    val exponent = -(deltaX * deltaX) / denominator
    if (exponent <= -690.0) 0.0 else normalizationFactor * Math.exp(exponent) // exp(x) < 1.0e-300 anyway
  }

  def derivative(x: Double): Double = (apply(x) * (average - x)) / derNormalizationFactor

}


final case class CumulativeNormalDistribution(average: Double = 0.0,
                                              sigma: Double = 1.0)
  extends GaussianDistribution(average, sigma) {

  private val gaussian = NormalDistribution()

  def apply(z: Double): Double = {
    val _z = (z - average) / sigma

    val result = 0.5 * (1.0 + ErrorFunction(_z * M_SQRT_2))

    if (result <= 1e-8) {
      //todo: investigate the threshold level
      // Asymptotic expansion for very negative z following (26.2.12)
      // on page 408 in M. Abramowitz and A. Stegun,
      // Pocketbook of Mathematical Functions, ISBN 3-87144818-4.
      var (sum, zsqr, i, g) = (1.0, _z * _z, 1.0, 1.0)
      var (x, y) = (0.0, 0.0)
      var (a, lasta) = (Double.MaxValue, 0.0)

      do {
        lasta = a
        x = (4.0 * i - 3.0) / zsqr
        y = x * ((4.0 * i - 1) / zsqr)
        a = g * (x - y)
        sum = sum - a
        g = g * y
        i = i + 1
        a = Math.abs(a)
      } while ((lasta > a) && (a >= Math.abs(sum * QL_EPSILON)))

      -gaussian(_z) / _z * sum
    } else {
      result
    }
  }

  def derivative(x: Double): Double = gaussian((x - average) / sigma) / sigma
}


final case class InverseCumulativeNormal(average: Double = 0.0,
                                         sigma: Double = 1.0) extends GaussianDistribution(average, sigma) {
  def apply(x: Double): Double = average + sigma * InverseCumulativeNormal.standardValue(x)
}

private object InverseCumulativeNormal {

  private val A = Seq(-3.969683028665376e+01,
    2.209460984245205e+02,
    -2.759285104469687e+02,
    1.383577518672690e+02,
    -3.066479806614716e+01,
    2.506628277459239e+00)

  private val B = Seq(-5.447609879822406e+01,
    1.615858368580409e+02,
    -1.556989798598866e+02,
    6.680131188771972e+01,
    -1.328068155288572e+01, 1.0)

  private val C = Seq(-7.784894002430293e-03,
    -3.223964580411365e-01,
    -2.400758277161838e+00,
    -2.549732539343734e+00,
    4.374664141464968e+00,
    2.938163982698783e+00)

  private val D = Seq(7.784695709041462e-03,
    3.224671290700398e-01,
    2.445134137142996e+00,
    3.754408661907416e+00, 1.0)

  private val x_low_ = 0.02425
  private val x_high_ = 1.0 - x_low_

  private def tailValue(x: Double): Double = {
    if (x <= 0.0 || x >= 1.0) {

      if (x ~= 1.0) {
        Double.MaxValue // largest value available
      } else if (x.abs < QL_EPSILON) {
        Double.MinValue // largest negative value available
      } else {
        assert(false, s"InverseCumulativeNormal($x) undefined: must be 0 < x < 1")
        Double.NaN
      }

    } else {

      val z = Math.sqrt(-2.0 * Math.log(if (x < x_low_) x else 1.0 - x))
      C.reduce(_ * z + _) / D.reduce(_ * z + _) * (if (x < x_low_) 1 else -1)
    }
  }

  def standardValue(x: Double): Double = {
    if (x < x_low_ || x_high_ < x) {
      tailValue(x)
    } else {
      val z = x - 0.5
      val r = z * z
      A.reduce(_ * r + _) * z / B.reduce(_ * r + _)
    }
  }

}

final case class MoroInverseCumulativeNormal(average: Double = 0.0, sigma: Double = 1.0) extends GaussianDistribution(average, sigma) {

  import MoroInverseCumulativeNormal._

  def apply(x: Double): Double = {
    require(x > 0.0 && x < 1.0, s"MoroInverseCumulativeNormal($x) undefined: must be 0<x<1");

    var result = 0.0
    val temp = x - 0.5

    if (Math.abs(temp) < 0.42) {
      // Beasley and Springer, 1977
      result = temp * temp
      result = temp * (((a3 * result + a2) * result + a1) * result + a0) / ((((b3 * result + b2) * result + b1) * result + b0) * result + 1.0)
    } else {
      // improved approximation for the tail (Moro 1995)
      var result = if (x < 0.5) x else 1.0 - x
      result = Math.log(-Math.log(result))
      result = c0 + result * (c1 + result * (c2 + result * (c3 + result * (c4 + result * (c5 + result * (c6 + result * (c7 + result * c8)))))))
      if (x < 0.5) result = -result
    }

    average + result * sigma
  }
}

object MoroInverseCumulativeNormal {
  val a0 = 2.50662823884
  val a1 = -18.61500062529
  val a2 = 41.39119773534
  val a3 = -25.44106049637

  val b0 = -8.47351093090
  val b1 = 23.08336743743
  val b2 = -21.06224101826
  val b3 = 3.13082909833

  val c0 = 0.3374754822726147
  val c1 = 0.9761690190917186
  val c2 = 0.1607979714918209
  val c3 = 0.0276438810333863
  val c4 = 0.0038405729373609
  val c5 = 0.0003951896511919
  val c6 = 0.0000321767881768
  val c7 = 0.0000002888167364
  val c8 = 0.0000003960315187

}

