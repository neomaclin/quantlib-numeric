package org.quantlib.math.distributions

import org.quantlib.math.Constants._

import scala.annotation.tailrec

object GammaFunction {

  private val C = List(
    76.18009172947146,
    -86.50532032941677,
    24.01409824083091,
    -1.231739572450155,
    0.1208650973866179e-2,
    -0.5395239384953e-5
  )

  def logValue(x: Double): Double = {
    require(x > 0.0, "positive argument required")

    val temp = (x + 5.5) - (x + 0.5) * Math.log(x + 5.5)
    val ser = C.zipWithIndex.map(c => c._1 / (x + (c._2 + 1.0))).sum + 1.000000000190015

    -temp + Math.log(2.5066282746310005 * ser / x)
  }

  def value(x: Double): Double = {
    if (x >= 1.0) {
      Math.exp(logValue(x))
    } else {
      if (x > -20.0) {
        // \Gamma(x) = \frac{\Gamma(x+1)}{x}
        value(x + 1.0) / x
      } else {
        // \Gamma(-x) = -\frac{\pi}{\Gamma(x)\sin(\pi x) x}
        -M_PI / (value(-x) * x * Math.sin(M_PI * x))
      }
    }
  }
}

final case class GammaDistribution(a: Double) extends (Double => Double) {
  require(a > 0.0, "invalid parameter for gamma distribution")

  override def apply(x: Double): Double = {
    if (x <= 0.0) {
      0.0
    }
    else {
      val gln = GammaFunction.logValue(a)

      if (x < (a + 1.0)) {

        @tailrec
        def sumUp(count: Int, ap: Double, del: Double, sum: Double): Double = {
          val ap_local: Double = ap + 1.0
          val del_local: Double = del * (x / ap_local)
          val sum_local: Double = sum + del_local
          if (Math.abs(del_local) < Math.abs(sum_local) * 3.0e-7 || count >= 100) sum_local * Math.exp(-x + a * Math.log(x) - gln)
          else sumUp(count + 1, ap_local, del_local, sum_local)
        }

        sumUp(1, a, 1.0 / a, 1.0 / a)

      } else {
        val b = x + 1.0 - a
        val c = QL_MAX_REAL
        val d = 1.0 / b
        val h = d


        @tailrec
        def sumUp(n: Int, b: Double, c: Double, d: Double, h: Double): Double = {
          val an = -1.0 * n * (n - a)
          val b_local = b + 2.0
          val d_local = if (Math.abs(an * d + b_local) < QL_EPSILON) QL_EPSILON else an * d + b_local
          val c_local = if (Math.abs(b_local + an / c) < QL_EPSILON) QL_EPSILON else b_local + an / c
          val del = (1.0 / d_local) * c_local
          val h_local = h * del
          if (Math.abs(del - 1.0) < QL_EPSILON) 1.0 - h_local * Math.exp(-x + a * Math.log(x) - gln)
          else sumUp(n + 1, b_local, c_local, 1.0 / d_local, h_local)
        }

        sumUp(1, b, c, d, h)
      }
    }
  }
}