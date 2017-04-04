package org.quantlib.math.functions

import org.quantlib.math.Constants
import org.quantlib.math.distributions._

/**
  * Created by Neo on 2016/7/30.
  */
final case class Gaussian(average: Double, sigma: Double) extends StatisticFunction {

  private val nd = NormalDistribution(average, sigma)
  private val cnd = CumulativeNormalDistribution(average, sigma)
  private val normFact = Constants.M_SQRT2 * Constants.M_SQRTPI

  def apply(x: Double): Double = nd(x) * normFact

  def derivative(x: Double): Double = nd.derivative(x) * normFact

  def primitive(x: Double): Double = cnd(x) * normFact
}
