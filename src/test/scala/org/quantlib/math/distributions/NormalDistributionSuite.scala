package org.quantlib.math.distributions

import org.scalatest._
import org.quantlib.math.Constants._
import org.quantlib.math.Utilities

/**
  * Created by neo on 04/04/2017.
  */
class NormalDistributionSuite extends FunSuite {
  val average = 1.0
  val sigma = 2.0

  def gaussian(x: Double): Double = {
    val normFact = sigma * Math.sqrt(2 * M_PI)
    val dx = x - average
    Math.exp(-dx * dx / (2.0 * sigma * sigma)) / normFact
  }

  def gaussianDerivative(x: Double): Double = {
    val normFact = sigma * sigma * sigma * Math.sqrt(2 * M_PI)
    val dx = x - average
    -dx * Math.exp(-dx * dx / (2.0 * sigma * sigma)) / normFact
  }

  val invCumStandardNormal = InverseCumulativeNormal()
  val normal = NormalDistribution(average, sigma)
  val cum = CumulativeNormalDistribution(average, sigma)
  val invCum = InverseCumulativeNormal(average, sigma)
  val mInvCum = MaddockInverseCumulativeNormal(average, sigma)

  val numberOfStandardDeviation = 6
  val xMin = average - numberOfStandardDeviation * sigma
  val xMax = average + numberOfStandardDeviation * sigma
  val N = 100001
  val h = (xMax - xMin) / (N - 1)

  test("Testing normal distributions...") {
    val check = invCumStandardNormal.apply(0.5)
    assert(check == 0.0, s"inverse cumulative of the standard normal at 0.5 is $check instead of 0.0")

    val x = (0 until N map (xMin + h * _)).toList

    val xNormal = x map normal
    val y = x map gaussian
    var diff = (y zip xNormal).map { case (a, b) => a - b }
    assert(Utilities.norm(diff, h) <= 1.0e-16,
      s"norm of NormalDistribution minus analytic Gaussian: ${Utilities.norm(diff, h)}. tolerance exceeded")

    val xId = x map (cum andThen invCum)
    diff = (x zip xId) map { case (a, b) => a - b }
    assert(Utilities.norm(diff, h) <= 1.0e-7,
      s"norm of invCum . cum minus identity: ${Utilities.norm(diff, h)}. tolerance exceeded")

    val xmId = x map (cum andThen mInvCum)
    diff = (x zip xId) map { case (a, b) => a - b }
    assert(Utilities.norm(diff, h) <= 1.0e-7,
      s"norm of MaddokInvCum . cum minus identity: ${Utilities.norm(diff, h)}. tolerance exceeded")

    val xD = x map cum.derivative
    diff = (xNormal zip xD) map { case (a, b) => a - b }
    assert(Utilities.norm(diff, h) <= 1.0e-16,
      s"norm of Cumulative.derivative minus analytic Gaussian: ${Utilities.norm(diff, h)}. tolerance exceeded")

    val xnormalD = x map normal.derivative
    val yd = x map gaussianDerivative
    diff = (yd zip xnormalD) map { case (a, b) => a - b }
    assert(Utilities.norm(diff, h) <= 1.0e-16,
      s"norm of Normal.derivative minus analytic derivative: ${Utilities.norm(diff, h)}. tolerance exceeded")

  }
}
