package org.quantlib.math

import org.quantlib.math.functions.Factorial

/**
  * Created by neo on 06/03/2017.
  */
package object distributions {

  def binomialCoefficientLn(n: Long, k: Long): Double = {
    require(n >= k, "n<k not allowed")

    Factorial.ln(n.toInt) - Factorial.ln(k.toInt) - Factorial.ln(n.toInt - k.toInt)

  }

  def binomialCoefficient(n: Long, k: Long): Double = {

    Math.floor(0.5 + Math.exp(binomialCoefficientLn(n, k)))

  }
  val gaussian = new NormalDistribution()
  val cumNormal = new CumulativeNormalDistribution()
  val invCumNormal = new InverseCumulativeNormal()
  // val chiSquareDistribution =


  def studentDistribution(n: Int, x: Double): Double = StudentDistribution(n)(x)
  def cumulativeStudentDistribution(n: Int, x: Double): Double = CumulativeStudentDistribution(n)(x)
  def inverseCumulativeStudent(n: Int, x: Double): Double = InverseCumulativeStudent(n)(x)

}
