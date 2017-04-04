package org.quantlib.math.distributions

import scala.annotation.tailrec
import org.quantlib.math.Constants._
import org.quantlib.math.functions.Beta
/**
  * Created by neo on 08/03/2017.
  */

final case class StudentDistribution(n: Int) extends (Double => Double) {
  require(n > 0, "invalid parameter for t-distribution")
  def apply(x: Double): Double = {
    import Math._
    val g1 = exp(GammaFunction.logValue(0.5 * (n + 1)))
    val g2 = exp(GammaFunction.logValue(0.5 * n))

    val power = pow (1.0 + x*x / n, 0.5 * (n + 1))

    g1 / (g2 * power * sqrt (M_PI * n))
  }
}

final case class CumulativeStudentDistribution(n: Int) extends (Double => Double) {
  require(n > 0, "invalid parameter for t-distribution")
  def apply(x: Double): Double = {
    val xx = 1.0 * n / (x*x + n)
    val sig = if (x > 0) 1.0 else - 1.0

    0.5 + 0.5 * sig * ( Beta.incompleteBetaFunction(0.5 * n, 0.5, 1.0) - Beta.incompleteBetaFunction(0.5 * n, 0.5, xx))
  }
}

final case class InverseCumulativeStudent(n: Int, accuracy: Double = 1e-6, maxIterations: Double = 50) extends (Double => Double) {
  val d = StudentDistribution(n)
  val f = CumulativeStudentDistribution(n)
  def apply(y: Double): Double = {
    require(y >= 0 && y <= 1, "argument out of range [0, 1]")

    // do a few newton steps to find x
    @tailrec
    def reduce(count: Int, result: Double): Double = {
      val local_result = result - (f(result) - y) / d(result)
      if (Math.abs(f.apply(local_result - y)) <= accuracy || count >= maxIterations) local_result
      else reduce(count+1, local_result)
    }
    val x = reduce(0, 0.0)

    x
  }
}
