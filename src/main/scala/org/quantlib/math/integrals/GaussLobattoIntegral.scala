package org.quantlib.math.integrals

import org.quantlib.math.Constants

/**
  * Created by neo on 28/02/2017.
  */
object GaussLobattoIntegral {
  val alpha = Math.sqrt(2.0 / 3.0)
  val beta = 1.0 / Math.sqrt(5.0)
  val x1 = 0.94288241569547971906
  val x2 = 0.64185334234578130578
  val x3 = 0.23638319966214988028


}

final case class GaussLobattoIntegral(absAccuracy: Double, maxIterations: Int,
                                      relAccuracy: Option[Double] = None,
                                      useConvergenceEstimate: Boolean = true) extends Integrator {

  import GaussLobattoIntegral._

  private var evalCount = 0

  private def calculateAbsTolerance(f: Double => Double, a: Double, b: Double): Double = {

    val m = (a + b) / 2
    val h = (b - a) / 2
    val y1 = f(a)
    val y3 = f(m - alpha * h)
    val y5 = f(m - beta * h)
    val y7 = f(m)
    val y9 = f(m + beta * h)
    val y11 = f(m + alpha * h)
    val y13 = f(b)

    val f1 = f(m - x1 * h)
    val f2 = f(m + x1 * h)
    val f3 = f(m - x2 * h)
    val f4 = f(m + x2 * h)
    val f5 = f(m - x3 * h)
    val f6 = f(m + x3 * h)

    val acc = h * (0.0158271919734801831 * (y1 + y13)
      + 0.0942738402188500455 * (f1 + f2)
      + 0.1550719873365853963 * (y3 + y11)
      + 0.1888215739601824544 * (f3 + f4)
      + 0.1997734052268585268 * (y5 + y9)
      + 0.2249264653333395270 * (f5 + f6)
      + 0.2426110719014077338 * y7)

    evalCount = evalCount + 13

    if (acc == 0.0 && (f1 != 0.0 || f2 != 0.0 || f3 != 0.0 || f4 != 0.0 || f5 != 0.0 || f6 != 0.0)) {
      new AssertionError("can not calculate absolute accuracy from relative accuracy")
    }

    var r = 1.0
    if (useConvergenceEstimate) {
      val integral2 = (h / 6) * (y1 + y13 + 5 * (y5 + y9))
      val integral1 = (h / 1470) * (77 * (y1 + y13) + 432 * (y3 + y11) + 625 * (y5 + y9) + 672 * y7)

      if (Math.abs(integral2 - acc) != 0.0)
        r = Math.abs(integral1 - acc) / Math.abs(integral2 - acc)
      if (r == 0.0 || r > 1.0)
        r = 1.0
    }

    val accuracy = relAccuracy.map { x => Math.max(x, Constants.QL_EPSILON) }.map { x => Math.min(absAccuracy, acc * x) }

    if (accuracy.nonEmpty) accuracy.get / (r * Constants.QL_EPSILON)
    else absAccuracy / (r * Constants.QL_EPSILON)

  }

  def adaptivGaussLobattoStep(f: Double => Double, a: Double, b: Double,
                              fa: Double, fb: Double, acc: Double): Double = {
    require(evalCount < maxIterations, "max number of iterations reached")

    val h = (b - a) / 2
    val m = (a + b) / 2

    val mll = m - alpha * h
    val ml = m - beta * h
    val mr = m + beta * h
    val mrr = m + alpha * h

    val fmll = f(mll)
    val fml = f(ml)
    val fm = f(m)
    val fmr = f(mr)
    val fmrr = f(mrr)

    evalCount = evalCount + 5

    val integral2 = (h / 6) * (fa + fb + 5 * (fml + fmr))
    val integral1 = (h / 1470) * (77 * (fa + fb) + 432 * (fmll + fmrr) + 625 * (fml + fmr) + 672 * fm)

    // avoid 80 bit logic on x86 cpu
    val dist = acc + (integral1 - integral2)
    if (dist == acc || mll <= a || b <= mrr) {
      require(m > a && b > m, "Interval contains no more machine number")
      integral1
    }
    else {
      adaptivGaussLobattoStep(f, a, mll, fa, fmll, acc)
      +adaptivGaussLobattoStep(f, mll, ml, fmll, fml, acc)
      +adaptivGaussLobattoStep(f, ml, m, fml, fm, acc)
      +adaptivGaussLobattoStep(f, m, mr, fm, fmr, acc)
      +adaptivGaussLobattoStep(f, mr, mrr, fmr, fmrr, acc)
      +adaptivGaussLobattoStep(f, mrr, b, fmrr, fb, acc)
    }
  }

  override def integrate(f: (Double) => Double, a: Double, b: Double): Double = {
    evalCount = 0
    val calcAbsTolerance = calculateAbsTolerance(f, a, b)

    evalCount = evalCount + 2

    adaptivGaussLobattoStep(f, a, b, f(a), f(b), calcAbsTolerance)
  }
}
