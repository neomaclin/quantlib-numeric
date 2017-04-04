package org.quantlib.math.functions

trait Curve extends (Double => Double)

object TestCurve extends Curve{
  def apply(x: Double): Double  = Math.sin(x)
}