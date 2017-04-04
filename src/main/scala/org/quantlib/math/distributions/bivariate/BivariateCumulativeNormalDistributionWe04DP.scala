package org.quantlib.math.distributions.bivariate

import org.quantlib.math.integrals.TabulatedGaussLegendre
import org.quantlib.math.integrals.TabulatedGaussLegendre._
import org.quantlib.math.Constants._
import org.quantlib.math.distributions.CumulativeNormalDistribution

/**
  * Created by neo on 04/04/2017.
  */

final case class BivariateCumulativeNormalDistributionWe04DP(rho: Double) extends BivariateDistribution {
  require(rho >= -1.0, s"rho must be >= -1.0 ($rho) not allowed")
  require(rho <= 1.0, s"rho must be <= 1.0 ($rho) not allowed)")
  private val cumnorm = CumulativeNormalDistribution()

  private def eqn3(h: Double, k: Double, asr: Double, x: Double): Double = {
    val hk = h * k
    val hs = (h * h + k * k) / 2
    val sn = Math.sin(asr * (-x + 1) * 0.5)
    Math.exp((sn * hk - hs) / (1.0 - sn * sn))
  }

  private def eqn6(a: Double, c: Double, d: Double, bs: Double, hk: Double, x: Double): Double = {
    var xs = a * (-x + 1)
    xs = Math.abs(xs * xs)
    val rs = Math.sqrt(1 - xs)
    val asr = -(bs / xs + hk) / 2
    if (asr > -100.0) {
      a * Math.exp(asr) * (Math.exp(-hk * (1 - rs) / (2 * (1 + rs))) / rs - (1 + c * xs * (1 + d * xs)))
    } else {
      0.0
    }
  }

  def apply(x: Double, y: Double): Double = {

    val gaussLegendreQuad =
      if (Math.abs(rho) < 0.3) TabulatedGaussLegendre(Six)
      else if (Math.abs(rho) < 0.75) TabulatedGaussLegendre(Twelve)
      else TabulatedGaussLegendre()

    val h = -x
    var k = -y
    var hk = h * k
    var BVN = 0.0

    if (Math.abs(rho) < 0.925) {
      if (Math.abs(rho) > 0) {
        val asr = Math.asin(rho)
        val f: Double => Double = eqn3(h, k, asr, _)
        BVN = gaussLegendreQuad(f)
        BVN *= asr * (0.25 / M_PI)
      }
      BVN += cumnorm(-h) * cumnorm(-k)
    } else {
      if (rho < 0) {
        k *= -1
        hk *= -1
      }
      if (Math.abs(rho) < 1) {
        val Ass = (1 - rho) * (1 + rho)
        var a = Math.sqrt(Ass)
        val bs = (h - k) * (h - k)
        val c = (4 - hk) / 8
        val d = (12 - hk) / 16
        val asr = -(bs / Ass + hk) / 2
        if (asr > -100) {
          BVN = a * Math.exp(asr) * (1 - c * (bs - Ass) * (1 - d * bs / 5) / 3 + c * d * Ass * Ass / 5)
        }
        if (-hk < 100) {
          val B = Math.sqrt(bs)
          BVN -= Math.exp(-hk / 2) * 2.506628274631 * cumnorm(-B / a) * B * (1 - c * bs * (1 - d * bs / 5) / 3)
        }
        a /= 2
        val f: Double => Double = eqn6(a, c, d, bs, hk, _)
        BVN += gaussLegendreQuad(f)
        BVN /= (-2.0 * M_PI)
      }

      if (rho > 0) {
        BVN += cumnorm(-Math.max(h, k))
      } else {
        BVN *= -1
        if (k > h) {
          // evaluate cumnorm where it is most precise, that
          // is in the lower tail because of double accuracy
          // around 0.0 vs around 1.0
          if (h >= 0) {
            BVN += cumnorm(-h) - cumnorm(-k)
          } else {
            BVN += cumnorm(k) - cumnorm(h)
          }
        }
      }
    }
    BVN
  }

}
