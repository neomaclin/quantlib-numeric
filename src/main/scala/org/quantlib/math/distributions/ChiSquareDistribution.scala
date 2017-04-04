package org.quantlib.math.distributions

import org.quantlib.math.Constants._

import scala.annotation.tailrec

/**
  * Created by neo on 06/03/2017.
  */
final case class ChiSquareDistribution(df: Double) extends (Double => Double) {
  def apply(x: Double): Double = GammaDistribution(0.5 * df)(0.5 * x)
}

final case class NonCentralChiSquareDistribution(df: Double, ncp: Double) extends (Double => Double) {
  def apply(x: Double): Double = {
    if (x <= 0.0) 0.0
    else {

      val errmax = 1e-12
      val itrmax = 10000
      val lam = 0.5 * ncp

      val x2 = 0.5 * x
      val f2 = 0.5 * df

      val t =
        if (f2 * QL_EPSILON > 0.125 && Math.abs(x2 - f2) < Math.sqrt(QL_EPSILON) * f2) {
          Math.exp((1 - 0.0) * (2 - 0.0 / (f2 + 1))) / Math.sqrt(2.0 * M_PI * (f2 + 1.0))
        } else {
          Math.exp(f2 * Math.log(x2) - x2 - GammaFunction.logValue(f2 + 1))
        }

      @tailrec
      def acc(n: Int, u: Double, v: Double, t: Double, ans: Double, f2n: Double, fx2n: Double): Double = {
        if (fx2n > 0) {
          if (t * x / fx2n <= errmax || n > itrmax) ans
          else acc(n, u, v, t, ans, f2n, fx2n)
        } else {
          val localU = u * lam / n
          val localV = v + localU
          val localT = t * x / f2n
          acc(n + 1, localU,localV, localT, ans + localV * localT, f2n + 2.0, fx2n + 2.0)
        }
      }

      acc(1, Math.exp(-lam) , Math.exp(-lam), t, Math.exp(-lam) * t ,df + 2.0, df - x + 2.0)
    }
  }
}

//final case class InverseNonCentralChiSquareDistribution(df: Double, ncp: Double,
//                                                        maxEvaluations: Int = 10,
//                                                        accuracy: Double = 1e-8) extends (Double => Double) {
//  private val nonCentralDist = NonCentralChiSquareDistribution(df, ncp)
//
//  def apply(x: Double): Double = {
//    var upper = df + ncp
//    var evaluations = maxEvaluations
//    while (nonCentralDist.apply(upper) < x && evaluations > 0) {
//      upper = upper * 2.0
//      evaluations = evaluations - 1
//    }
//
//    val solver = Brent()
//    solver.solve(
//      compose(std :: bind2nd(std :: minus < Real > (), x), nonCentralDist),
//      accuracy,
//      0.75 * upper,
//      if (evaluations == maxEvaluations) 0.0 else 0.5 * upper,
//      upper)
//
//  }
//}