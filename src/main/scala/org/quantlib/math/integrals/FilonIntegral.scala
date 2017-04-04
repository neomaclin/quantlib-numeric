package org.quantlib.math.integrals

import org.quantlib.math.integrals.FilonIntegral._

/**
  * Created by neo on 03/03/2017.
  */
object FilonIntegral {

  sealed trait Choice

  object Choice {

    case object Sine extends Choice

    case object Cosine extends Choice

  }
}

final case class FilonIntegral(functionChoice: Choice, k: Double, interval: Int) extends Integrator {
  require(interval % 2 == 0, "number of intervals must be even")
  private val n: Int = interval / 2

  def integrate(f: Double => Double, l: Double, u: Double): Double = {

    val h = (u - l) / interval

    val x = l until (u+h, h)
    val y = x.map(f)

    val q = k * h
    val q2 = q * q
    val q3 = q * q * q

    val alpha = (q2 + q * Math.sin(2.0 * q) / 2.0 - 2.0 * (Math.sin(q) * Math.sin(q))) / q3
    val beta = 2.0 * (q * (1.0 + (Math.cos(q) * Math.cos(q))) - Math.sin(2.0 * q)) / q3
    val gamma = 4.0 * ( Math.sin(q) - q * Math.cos(q) ) / q3

    val f1: Double => Double = if (functionChoice == Choice.Cosine) Math.sin else Math.cos
    val f2: Double => Double = if (functionChoice == Choice.Cosine) Math.cos else Math.sin

    val sum_odd = (1 to n).map{ i => y(2 * i - 1) * f2(k * x(2 * i - 1)) }.sum

    val sum_even = y(0) * f2(k * l) - 0.5 * (y(interval) * f2(k * u) + y(0) * f2(k * l)) +
      (1 to n).map{ i => y(2 * i) * f2(k * x(2 * i)) }.sum

    val p = if (functionChoice == Choice.Cosine) 1.0 else -1.0

    h * (p * alpha * (y(interval) * f1(k * u) - y(0) * f1(k * l)) + beta * sum_even + gamma * sum_odd)
  }
}