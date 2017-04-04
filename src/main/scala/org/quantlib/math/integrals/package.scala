package org.quantlib.math

import org.quantlib.math.integrals.Integrator._
import org.quantlib.math.integrals.TrapezoidIntegrals._

import scala.annotation.tailrec

/**
  * Created by neo on 28/02/2017.
  */
package object integrals {


  private val defaultAccurracy = 1.0e-6
  val segment: Integrator = SegmentIntegral()
  val trapezoid: Integrator = TrapezoidIntegral(defaultAccurracy, 1024)
  val simpson: Integrator = SimpsonIntegral(defaultAccurracy, 1024)


  sealed trait IntegrationPolicy {

    def integrate(f: Double => Double, a: Double, b: Double, I: Double, N: Int): Double

    def nbEvalutions: Int

  }

  object IntegrationPolicy {

    case object DefaultPolicy extends IntegrationPolicy {
      def integrate(f: Double => Double, a: Double, b: Double, I: Double, N: Int): Double = {
        val dx: Double = (b - a) / N

        @tailrec
        def sum(acc: Int, x: Double, result: Double): Double = {
          if (acc >= N) result
          else sum(acc + 1, x + dx, result + f(x))
        }

        (I + dx * sum(1, a + dx / 2.0, 0.0)) / 2.0
      }

      val nbEvalutions = 2
    }

    case object MidPointPolicy extends IntegrationPolicy {
      def integrate(f: Double => Double, a: Double, b: Double, I: Double, N: Int): Double = {
        val dx = (b - a) / N
        val D = 2.0 * dx / 3.0

        @tailrec
        def sum(acc: Int, x: Double, result: Double): Double = {
          if (acc >= N) result
          else sum(acc + 1, x + dx, result + f(x) + f(x + D))
        }

        (I + dx * sum(1, a + dx / 6.0, 0.0)) / 3.0

      }

      val nbEvalutions = 3
    }

  }

}
