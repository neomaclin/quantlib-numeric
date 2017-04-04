package org.quantlib.math.integrals

import org.quantlib.math.integrals.DiscreteIntegrals._

/**
  * Created by neo on 28/02/2017.
  */

sealed abstract class DiscreteIntegrals(evaluations: Int) extends Integrator

final case class DiscreteTrapezoidIntegrator(evaluations: Int) extends DiscreteIntegrals(evaluations) {

  def integrate(f: Double => Double, a: Double, b: Double): Double = {
    val step = (b - a) / (evaluations - 1)
    val x: Seq[Double] = (0 until evaluations).map { i => a + step * i }
    val fv: Seq[Double] = x.map(f)

    DiscreteTrapezoidIntegral(x, fv)
  }
}

final case class DiscreteSimpsonIntegrator(evaluations: Int) extends DiscreteIntegrals(evaluations) {
  def integrate(f: (Double) => Double, a: Double, b: Double): Double = {
    val step = (b - a) / (evaluations - 1)
    val x: Seq[Double] = (0 until evaluations).map { i => a + step * i }
    val fv: Seq[Double] = x.map(f)

    DiscreteSimpsonIntegral(x, fv)
  }
}

object DiscreteIntegrals {

  case object DiscreteTrapezoidIntegral {
    def apply(x: Seq[Double], f: Seq[Double]): Double = {
      require(x.length == f.length, "inconsistent size")
      var sum: Double = 0.0

      (0 until x.length - 1) foreach { i =>
        sum = sum + 0.5 * ((x(i + 1) - x(i)) * (f(i) + f(i + 1)))
      }

      sum

    }
  }

  case object DiscreteSimpsonIntegral {
    def apply(x: Seq[Double], f: Seq[Double]): Double = {
      require(x.length == f.length, "inconsistent size")
      val n = x.length
      var sum = 0.0

      (0 until(n - 2, 2)) foreach { j =>
        val dxj = x(j + 1) - x(j)
        val dxjp1 = x(j + 2) - x(j + 1)

        val alpha = -dxjp1 * (2 * x(j) - 3 * x(j + 1) + x(j + 2))
        val dd = x(j + 2) - x(j)
        val k = dd / (6 * dxjp1 * dxj)
        val beta = dd * dd
        val gamma = dxj * (x(j) - 3 * x(j + 1) + 2 * x(j + 2))

        sum = sum + k * alpha * f(j) + k * beta * f(j + 1) + k * gamma * f(j + 2)
      }

      if (n % 2 == 0) {
        sum = sum + 0.5 * (x(n - 1) - x(n - 2)) * (f(n - 1) + f(n - 2))
      }

      sum
    }
  }

}
