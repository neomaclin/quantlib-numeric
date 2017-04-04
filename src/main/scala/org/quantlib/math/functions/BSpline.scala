package org.quantlib.math.functions

/**
  * Created by neo on 23/03/2017.
  */
final case class BspLine(n: Int,
                         p: Int,
                         knots: Seq[Double]) {
  require(p >= 1, "lowest degree B-spline has p = 1")
  require(n >= 1, "number of control points n+1 >= 2")
  require(p <= n, "must have p <= n")
  require(knots.size == p + n + 2, "number of knots must equal p+n+2")
  require(knots == knots.sortWith(_ <= _), "knots points must be nondecreasing")

  def apply(i: Int, x: Double): Double = {
    require(i <= n, "i must not be greater than n")

    N(i, p, x)
  }

  def N(i: Int, p: Int, x: Double): Double = {
    if (p == 0) {
      if (knots(i) <= x && x < knots(i + 1)) 1.0 else 0.0
    } else {
      ((x - knots(i)) / (knots(i + p) - knots(i))) * N(i, p - 1, x) +
        ((knots(i + p + 1) - x) / (knots(i + p + 1) - knots(i + 1))) * N(i + 1, p - 1, x)
    }
  }

}