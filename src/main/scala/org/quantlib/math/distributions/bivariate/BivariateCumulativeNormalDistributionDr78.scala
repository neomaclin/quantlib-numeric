package org.quantlib.math.distributions.bivariate

import org.quantlib.math.distributions.CumulativeNormalDistribution
import org.quantlib.math.Constants._

/**
  * Created by neo on 04/04/2017.
  */
final case class BivariateCumulativeNormalDistributionDr78(rho: Double) extends BivariateDistribution {
  require(rho >= -1.0, s"rho must be >= -1.0 ($rho) not allowed")
  require(rho <= 1.0, s"rho must be <= 1.0 ($rho) not allowed)")

  private val rhoSquare = rho * rho
  private val cumNormalDist = CumulativeNormalDistribution()

  def apply(a: Double, b: Double): Double = {

    val CumNormDistA = cumNormalDist(a)
    val CumNormDistB = cumNormalDist(b)
    val MaxCumNormDistAB = Math.max(CumNormDistA, CumNormDistB)
    val MinCumNormDistAB = Math.min(CumNormDistA, CumNormDistB)

    if (1.0 - MaxCumNormDistAB < 1e-15 || MinCumNormDistAB < 1e-15) {
      MinCumNormDistAB
    } else {

      val a1 = a / Math.sqrt(2.0 * (1.0 - rhoSquare))
      val b1 = b / Math.sqrt(2.0 * (1.0 - rhoSquare))

      if (a <= 0.0 && b <= 0.0 && rho <= 0.0) {
        import BivariateCumulativeNormalDistributionDr78._

        val mapping = for {
          (a, i) <- x.zipWithIndex
          (b, j) <- y.zipWithIndex
        } yield {
          a * x(j) * Math.exp(a1 * (2.0 * y(i) - a1) + b1 * (2.0 * b - b1) + 2.0 * rho * (y(i) - a1) * (b - b1))
        }

        Math.sqrt(1.0 - rhoSquare) / M_PI * mapping.sum
      } else if (a <= 0.0 && b >= 0.0 && rho >= 0.0) {
        CumNormDistA - BivariateCumulativeNormalDistributionDr78(-rho).apply(a, -b)
      } else if (a >= 0.0 && b <= 0.0 && rho >= 0.0) {
        CumNormDistB - BivariateCumulativeNormalDistributionDr78(-rho).apply(-a, b)
      } else if (a >= 0.0 && b >= 0.0 && rho <= 0.0) {
        CumNormDistA + CumNormDistB - 1.0 + this.apply(-a, -b)
      } else if (a * b * rho > 0.0) {
        val aSide = if (a > 0.0) 1.0 else -1.0
        val bSide = if (b > 0.0) 1.0 else -1.0

        val rho1 = (rho * a - b) * aSide / Math.sqrt(a * a - 2.0 * rho * a * b + b * b)
        val bivCumNormalDist = BivariateCumulativeNormalDistributionDr78(rho1)

        val rho2 = (rho * b - a) * bSide / Math.sqrt(a * a - 2.0 * rho * a * b + b * b)
        val CBND2 = BivariateCumulativeNormalDistributionDr78(rho2)

        val delta = (1.0 - aSide * bSide) / 4.0

        bivCumNormalDist(a, 0.0) + CBND2(b, 0.0) - delta
      } else {
        -1.0
      }

    }

  }
}


object BivariateCumulativeNormalDistributionDr78 {
  private val x = Seq(
    0.24840615,
    0.39233107,
    0.21141819,
    0.03324666,
    0.00082485334
  )

  private val y = Seq(
    0.10024215,
    0.48281397,
    1.06094980,
    1.77972940,
    2.66976040000
  )
}