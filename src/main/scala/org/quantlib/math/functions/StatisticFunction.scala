package org.quantlib.math.functions

/**
  * Created by Neo on 2016/7/30.
  */
trait StatisticFunction extends (Double => Double) {
  def derivative(x: Double): Double
  def primitive(x: Double): Double
}
