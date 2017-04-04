package org.quantlib.math.ode

/**
  * Created by neo on 03/03/2017.
  */
object AdaptiveRungeKutta {
  private val (a2, a3, a4, a5, a6) = (0.2, 0.3, 0.6, 1.0, 0.875)
  private val b21 = 0.2
  private val (b31, b32) = (3.0/40.0, 9.0/40.0)
  private val (b41, b42, b43) = (0.3, -0.9, 1.2)
  private val (b51, b52, b53, b54) = (-11.0 / 54.0, 2.5, -70.0 / 27.0, 35.0 / 27.0)
  private val (b61, b62, b63, b64, b65) = (1631.0 / 55296.0, 175.0 / 512.0, 575.0 / 13824.0, 44275.0 / 110592.0, 253.0 / 4096.0)
  private val (c1, c3, c4, c6) = (37.0 / 378.0, 250.0 / 621.0, 125.0 / 594.0, 512.0 / 1771.0)
  private val (dc1, dc3, dc4, dc5, dc6) = (c1 - 2825.0 / 27648.0, c3 - 18575.0 / 48384.0, c4 - 13525.0 / 55296.0, -277.0 / 14336.0, c6 - 0.25)
 // private val (adaptiverk_maxstp, adaptiverk_tiny, adaptiverk_safety) = (10000, 1.0E-30, 0.9)
//  private val (adaptiverk_pgrow, adaptiverk_pshrink, adaptiverk_errcon) = (-0.2, -0.25, 1.89E-4)
}

final case class AdaptiveRungeKutta(eps: Double = 1.0e-6,
                                       h1: Double = 1.0e-4,
                                       hmin: Double = 0.0) {

  import AdaptiveRungeKutta._

  private def wrapper[Double](func: Double => Double => Double,
                              x: Double, y: Vector[Double]): Vector[Double] = Vector(func(x)(y.head))


//  private def rkqs(y: Vector[Double], dydx: Vector[Double], x: Double, htry: Double,
//                   eps: Double, yScale: Vector[Double], hdid: Double, hnext: Double,
//                   derivs: (Double, Vector[Double]) => Vector[Double]): Vector[Double] = {
//   // while () {}
//   //
//    def check(): Unit ={
//     for (Size i=0;i<n;i++)
//     val (ytemp, yerr) = rkck(y, dydx, x, htry, derivs)
//     errmax=Math.max(errmax,Math.abs(yerr(i)/yScale(i));
//     errmax/=eps;
//   }
//  }

  private def rkck(y: Vector[Double], dydx: Vector[Double],
                   x: Double, h: Double,
                   derivs: (Double, Vector[Double]) => Vector[Double]): (Vector[Double], Vector[Double]) = {


    val ak2 = derivs(x + a2 * h, (y zip dydx).map{ case (a, b) => a + h * b21 * b })
    val ak3 = derivs(x + a3 * h, (y zip dydx zip ak2).map{ case ((a, b), c) => a + h * (b31 * b + b32 * c) })
    val ak4 = derivs(x + a4 * h, (y zip dydx zip ak2 zip ak3).map{ case (((a, b), c), d) => a + h * (b41 * b + b42 * c + b43 * d) })
    val ak5 = derivs(x + a5 * h, (y zip dydx zip ak2 zip ak3 zip ak4).map{ case ((((a, b), c), d), e) => a + h * (b51 * b + b52 * c + b53 * d + b54 * e) })
    val ak6 = derivs(x + a6 * h, (y zip dydx zip ak2 zip ak3 zip ak4 zip ak5).map{ case (((((a, b), c), d), e), f) => a + h * (b61 * b + b62 * c + b63 * d + b64 * e + b65 * f) })

    val yout = (y zip dydx zip ak3 zip ak4 zip ak6).map { case ((((a, b), c), d), e) => a + h * (c1 * b + c3 * c + c4 * d + c6 * e) }
    val yerror = (dydx zip ak3 zip ak4 zip ak5 zip ak6).map { case ((((b, c), d), e), f) => h * (dc1 * b + dc3 * c + dc4 * d + dc5 * e + dc6 * f) }
    (yout, yerror)
  }

//  def apply(ode: Double => Double => Vector[Double], y1: Vector[Double], x1: Double, x2: Double): Vector[Double] = {
//
//
//  }
//
//  def apply(ode: (Double, T) => Double, y1: T, x1: Double, x2: Double): T = {
//    apply(wrapper(ode), Vector[Double](y1), x1, x2)(0)
//  }

}