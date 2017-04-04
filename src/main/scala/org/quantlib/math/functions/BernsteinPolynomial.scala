package org.quantlib.math.functions

/**
  * Created by neo on 23/03/2017.
  */
object BernsteinPolynomial {
  def get(i: Int, n: Int, x: Double):Double = {
    val coeff = Factorial.get(n) / (Factorial.get(n-i) * Factorial.get(i))

    coeff * Math.pow(x,i) * Math.pow(1.0-x, n-i)
  }
}
