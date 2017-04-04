package org.quantlib.math.distributions.bivariate

import org.quantlib.math.Constants._
/**
  * Created by neo on 04/04/2017.
  */
final case class BivariateCumulativeStudentDistribution(n: Int, rho: Double) extends BivariateDistribution {

  private def sign(value: Double): Double = if (value == 0.0) 0.0 else if (value < 0.0) -1.0 else 1.0
  
  private def arctan(x: Double, y: Double): Double =  {
    val res = Math.atan2(x, y)
    if (res >= 0.0) res else res + 2 * M_PI
  }

  // function x(m,h,k) defined on top of page 155
  private def fx(m: Double,  h: Double,  k: Double, rho: Double): Double =  {
    val unCor = 1 - rho * rho
    val sub = Math.pow(h - rho * k, 2)
    val denom = sub + unCor * (m + k * k)
    if (denom < QL_EPSILON) 0.0 // limit case for rho = +/-1.0
    else sub / (sub + unCor * (m + k * k))
  }

  private def pn(h: Double, k: Double, n: Int, rho: Double): Double = {
    val unCor = 1.0 - rho*rho

    val div = 4 * Math.sqrt(n * M_PI)
    val xHK = fx(n, h, k, rho)
    val xKH = fx(n, k, h, rho)
    val divH = 1 + h*h / n
    val divK = 1 + k*k / n
    val sgnHK = sign(h - rho * k)
    val sgnKH = sign(k - rho * h)

    if (n % 2 == 0) { // n is even, equation (10)
      // first line of (10)
      var res = arctan(Math.sqrt(unCor), -rho) / M_TWOPI

      // second line of (10)
      var dgM = 2 * (1 - xHK)  // multiplier for dgj
      var gjM = sgnHK * 2 / M_PI // multiplier for g_j
      // initializations for j = 1:
      var f_j = Math.sqrt(M_PI / divK)
      var g_j = 1 + gjM * arctan(Math.sqrt(xHK), Math.sqrt(1 - xHK))
      var sum = f_j * g_j
      if (n >= 4) {
        // different formulas for j = 2:
        f_j *= 0.5 / divK // (2 - 1.5) / (Real) (2 - 1) / divK
        var dgj = gjM * Math.sqrt(xHK * (1 - xHK))
        g_j += dgj
        sum += f_j * g_j
        // and then the loop for the rest of the j's:
        3 to (n / 2) foreach { j =>
          f_j *= (j - 1.5) / (j - 1.0) / divK
          dgj *= (j - 2.0) / (2.0 * j - 3.0) * dgM
          g_j += dgj
          sum += f_j * g_j
        }
      }
      res += k / div * sum

      // third line of (10)
      dgM = 2 * (1 - xKH)
      gjM = sgnKH * 2 / M_PI
      // initializations for j = 1:
      f_j = Math.sqrt(M_PI / divH)
      g_j = 1 + gjM * arctan(Math.sqrt(xKH), Math.sqrt(1 - xKH))
      sum = f_j * g_j
      if (n >= 4) {
        // different formulas for j = 2:
        f_j *= 0.5 / divH // (2 - 1.5) / (Real) (2 - 1) / divK
        var dgj = gjM * Math.sqrt(xKH * (1 - xKH))
        g_j += dgj
        sum += f_j * g_j
        // and then the loop for the rest of the j's:
        3 to (n / 2) foreach { j =>
          f_j *= (j - 1.5) / (j - 1.0) / divH
          dgj *= (j - 2.0) / (2.0 * j - 3.0) * dgM
          g_j += dgj
          sum += f_j * g_j
        }
      }
      res += h / div * sum
      res

    } else { // n is odd, equation (11)
      // first line of (11)
      var hk = h * k
      var hkcn = hk + rho * n
      var sqrtExpr = Math.sqrt(h*h - 2 * rho * hk + k*k + n * unCor)
      var res = arctan(Math.sqrt(n) * (-(h + k) * hkcn - (hk - n) * sqrtExpr),
        (hk - n) * hkcn - n * (h + k) * sqrtExpr ) / M_TWOPI

      if (n > 1) {
        // second line of (11)
        var mult = (1 - xHK) / 2
        // initializations for j = 1:
        var f_j = 2 / Math.sqrt(M_PI) / divK
        var dgj = sgnHK * Math.sqrt(xHK)
        var g_j = 1 + dgj
        var sum = f_j * g_j
        // and then the loop for the rest of the j's:
        2 to ((n - 1) / 2) foreach { j =>
          f_j *=  (j - 1.0) / (j - 0.5) / divK
          dgj *=  (2.0 * j - 3.0) / (j - 1.0) * mult
          g_j += dgj
          sum += f_j * g_j
        }
        res += k / div * sum

        // third line of (11)
        mult = (1 - xKH) / 2
        // initializations for j = 1:
        f_j = 2 / Math.sqrt(M_PI) / divH
        dgj = sgnKH * Math.sqrt(xKH)
        g_j = 1 + dgj
        sum = f_j * g_j
        // and then the loop for the rest of the j's:
        2 to ((n - 1) / 2) foreach { j =>
          f_j *= (j - 1.0) / (j - 0.5) / divH
          dgj *= (2.0 * j - 3.0) / (j - 1.0) * mult
          g_j += dgj
          sum += f_j * g_j
        }
        res += h / div * sum
      }
      res
    }
  }

  def apply(x: Double, y: Double): Double = pn(x,y,n,rho)

}
