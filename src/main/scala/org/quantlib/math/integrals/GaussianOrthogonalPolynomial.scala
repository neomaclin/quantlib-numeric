package org.quantlib.math.integrals

import org.quantlib.math.Constants
import org.quantlib.math.distributions.GammaFunction

import org.quantlib.math.Comparing._

/**
  * Created by neo on 03/03/2017.
  */
sealed trait GaussianOrthogonalPolynomial {

  def mu0: Double

  def alpha(i: Int): Double

  def beta(i: Int): Double

  def w(x: Double): Double

  def value(n: Int, x: Double): Double = {
    if (n > 1) {
      (x - alpha(n - 1)) * value(n - 1, x) - beta(n - 1) * value(n - 2, x)
    } else if (n == 1) {
      x - alpha(0)
    } else 1
  }

  def weightedValue(n: Int, x: Double): Double = Math.sqrt(w(x)) * value(n, x)

}


final case class GaussLaguerrePolynomial(s: Double = 0.0) extends GaussianOrthogonalPolynomial {
  require(s > -1.0, "s must be bigger than -1")

  def mu0: Double = Math.exp(GammaFunction.logValue(s + 1))

  def alpha(i: Int): Double = 2 * i + 1 + s

  def beta(i: Int): Double = i * (i + s)

  def w(x: Double): Double = Math.pow(x, s) * Math.exp(-x)
}

//! Gauss-Hermite polynomial
final case class GaussHermitePolynomial(mu: Double = 0.0) extends GaussianOrthogonalPolynomial {
  require(mu > -0.5, "mu must be bigger than -0.5")

  def mu0: Double = Math.exp(GammaFunction.logValue(mu + 0.5))

  def alpha(i: Int): Double = 0.0

  def beta(i: Int): Double = if (i % 2 == 0) i / 2.0 + mu else i / 2.0

  def w(x: Double): Double = Math.pow(Math.abs(x), 2 * mu) * Math.exp(-x * x)

}

//! Gauss-Jacobi polynomial
sealed abstract class GaussJacobiPolynomial(alpha: Double, beta: Double) extends GaussianOrthogonalPolynomial {
  require(alpha + beta > -2.0, "alpha+beta must be bigger than -2")
  require(alpha > -1.0, "alpha must be bigger than -1")
  require(beta > -1.0, "beta  must be bigger than -1")

  def mu0: Double = {
    Math.pow(2.0, alpha + beta + 1) *
      Math.exp(GammaFunction.logValue(alpha + 1) +
        GammaFunction.logValue(beta + 1) -
        GammaFunction.logValue(alpha + beta + 2))
  }

  def alpha(i: Int): Double = {

      val num = beta *beta - alpha*alpha
      val denom = (2.0*i+alpha+beta)*(2.0*i+alpha+beta+2)

      if ( denom ~= 0.0 ) {
        if (!(num ~= 0.0)) {
          new AssertionError("can't compute ak for jacobi integration\n")
          num / denom
        } else {
          // l'Hospital
          val fixnum  = 2*beta
          val fixdenom= 2*(2.0*i+alpha+beta+1)

          assert(!(fixdenom ~= 0.0), "can't compute ak for jacobi integration\n")
          fixnum / fixdenom
        }
      } else num / denom

  }

  def beta(i: Int): Double = {
    val num = 4.0*i*(i+alpha)*(i+beta)*(i+alpha+beta)
    val denom = (2.0*i+alpha+beta)*(2.0*i+alpha+beta) * ((2.0*i+alpha+beta)*(2.0*i+alpha+beta)-1)

    if ( denom ~= 0.0 ) {
      if (!(num ~= 0.0)) {
        new AssertionError("can't computebk for jacobi integration\n")
      } else {
        // l'Hospital
        val fixnum = 4.0*i*(i+beta)* (2.0*i+2*alpha+beta)
        var fixdenom = 2.0*(2.0*i+alpha+beta)
        fixdenom = fixdenom * (denom-1)
        assert(!(fixdenom  ~= 0.0), "can't compute bk for jacobi integration\n")
        fixnum / fixdenom
      }
    }
     num / denom
  }

  def w(x: Double): Double = Math.pow(1-x, alpha)*Math.pow(1+x, beta)

}


final case class GeneralGaussJacobiPolynomial(alpha: Double, beta: Double) extends GaussJacobiPolynomial(alpha,beta)
//! Gauss-Legendre polynomial
case object GaussLegendrePolynomial extends GaussJacobiPolynomial(0.0,0.0)

//! Gauss-Chebyshev polynomial
case object GaussChebyshevPolynomial extends GaussJacobiPolynomial(0.5,0.5)

//! Gauss-Chebyshev polynomial (second kind)
case object GaussChebyshev2ndPolynomial extends GaussJacobiPolynomial(-0.5, -0.5)

//! Gauss-Gegenbauer polynomial
final case class GaussGegenbauerPolynomial(lambda: Double) extends GaussJacobiPolynomial(lambda-0.5, lambda-0.5)

//! Gauss hyperbolic polynomial
case object GaussHyperbolicPolynomial extends GaussianOrthogonalPolynomial {

  override def mu0: Double = Constants.M_PI

  override def alpha(i: Int): Double = 0.0

  override def beta(i: Int): Double = if (i != 0) Constants.M_PI_2 * Constants.M_PI_2 * i * i else Constants.M_PI

  override def w(x: Double): Double = 1 / Math.cosh(x)
}

