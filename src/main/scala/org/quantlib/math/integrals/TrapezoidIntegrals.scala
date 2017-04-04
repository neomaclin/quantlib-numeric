package org.quantlib.math.integrals

import org.quantlib.math.integrals.IntegrationPolicy.DefaultPolicy

import scala.annotation.tailrec

/**
  * Created by neo on 28/02/2017.
  */
trait TrapezoidIntegrals extends Integrator

object TrapezoidIntegrals {

  final case class TrapezoidIntegral(accuracy: Double,
                                     maxIterations: Int,
                                     policy: IntegrationPolicy = DefaultPolicy) extends TrapezoidIntegrals {
    def integrate(f: Double => Double, a: Double, b: Double): Double = {

      @tailrec
      def evaluation(i: Int, I: Double, N: Int): Double = {
        require(i < maxIterations, "max number of iterations reached")
        val newI = policy.integrate(f, a, b, I, N)
        if ((Math.abs(I - newI) <= accuracy) && (i > 5)) newI else evaluation(i + 1, newI, N * policy.nbEvalutions)
      }

      evaluation(1, (f(a) + f(b)) * (b - a) / 2.0, 1)
    }
  }


  final case class SimpsonIntegral(accuracy: Double,
                                   maxIterations: Int,
                                   policy: IntegrationPolicy = DefaultPolicy) extends TrapezoidIntegrals {
    def integrate(f: Double => Double, a: Double, b: Double): Double = {

      val I = (f(a) + f(b)) * (b - a) / 2.0
      val adjI = I

      @tailrec
      def evaluation(i: Int, I: Double, adjI: Double, N: Int): Double = {
        require(i < maxIterations, "max number of iterations reached")
        val newI = policy.integrate(f, a, b, I, N)
        val newAdjI  = (4.0 * newI - I) / 3.0
        if ((Math.abs(adjI - newAdjI) <= accuracy) && (i > 5)) newAdjI else evaluation(i + 1, newI, newAdjI, N * 2)
      }

      evaluation(1, I, adjI, 1)
    }
  }
}
