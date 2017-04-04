package org.quantlib.math.integrals


import org.quantlib.math.integrals.TabulatedGaussLegendre._
import org.quantlib.math.matrix.TqrEigenDecomposition
import org.quantlib.math.matrix.TqrEigenDecomposition.EigenVectorCalculation._
import org.quantlib.math.matrix.TqrEigenDecomposition.ShiftStrategy._


sealed class GaussianQuadrature(n: Int,
                                private val p: GaussianOrthogonalPolynomial) extends ((Double => Double) => Double) {

  private val tqr = TqrEigenDecomposition (
    (0 until n).map(p.alpha),
    (0 until n-1).map(i=> Math.sqrt(p.beta(i+1))),
    OnlyFirstRowEigenVector,
    Overrelaxation)

  private val ev = tqr.eigenvectors

  private val mu0 = p.mu0
  val x = tqr.eigenvalues
  val weights: Seq[Double] = (ev.head zip x).map{case (wv,xv) => mu0 * wv * wv / p.w(wv)}

  def apply(f: Double => Double): Double = (x zip weights).reverseMap { case (xv, wv) => f(xv) * wv }.sum

}

final case class GaussLaguerreIntegration(n: Int, s: Double = 0.0) extends GaussianQuadrature(n, GaussLaguerrePolynomial(s))
final case class GaussHermiteIntegration(n: Int, mu: Double = 0.0) extends GaussianQuadrature(n, GaussHermitePolynomial(mu))
final case class GaussJacobiIntegration(n: Int, alpha: Double, beta: Double) extends GaussianQuadrature(n, GeneralGaussJacobiPolynomial(alpha,beta))
final case class GaussLegendreIntegration(n: Int) extends GaussianQuadrature(n, GaussLegendrePolynomial)
final case class GaussChebyshevIntegration(n: Int) extends GaussianQuadrature(n, GaussChebyshevPolynomial)
final case class GaussHyperbolicIntegration(n: Int) extends GaussianQuadrature(n, GaussHyperbolicPolynomial)
final case class GaussChebyshev2ndIntegration(n: Int) extends GaussianQuadrature(n, GaussChebyshev2ndPolynomial)
final case class GaussGegenbauerIntegration(n: Int,lambda:Double) extends GaussianQuadrature(n, GaussGegenbauerPolynomial(lambda))


final case class TabulatedGaussLegendre(order: TabulateOrder = Twenty) extends ((Double => Double) => Double) {
  private val (w, x, n) = order match {
    case Six => (x6, w6, n6)
    case Seven => (x7, w7, n7)
    case Twelve => (x12, w12, n12)
    case Twenty => (x20, w20, n20)
  }

  def apply(f: (Double) => Double): Double = order match {
      case Seven =>
        w(0) * f(x(0)) + (w.tail zip x.tail).map { case (xv, wv) => f(xv) * wv + f(-xv) * wv }.sum
      case _ =>
        (w zip x).map { case (xv, wv) => f(xv) * wv + f(-xv) * wv }.sum
    }

}


object TabulatedGaussLegendre {

  //def Size

  sealed trait TabulateOrder

  case object Six extends TabulateOrder

  case object Seven extends TabulateOrder

  case object Twelve extends TabulateOrder

  case object Twenty extends TabulateOrder

  // Abscissas and Weights from Abramowitz and Stegun

  /* order 6 */
  private val x6 = Seq[Double](0.238619186083197,
    0.661209386466265,
    0.932469514203152)

  private val w6 = Seq[Double](0.467913934572691,
    0.360761573048139,
    0.171324492379170)

  private  val n6 = 3

  /* order 7 */
  private  val x7 = Seq[Double](0.000000000000000,
    0.405845151377397,
    0.741531185599394,
    0.949107912342759)

  private val w7 = Seq[Double](0.417959183673469,
    0.381830050505119,
    0.279705391489277,
    0.129484966168870)

  private val n7 = 4

  /* order 12 */
  private val x12 = Seq[Double](0.125233408511469,
    0.367831498998180,
    0.587317954286617,
    0.769902674194305,
    0.904117256370475,
    0.981560634246719)

  private val w12 = Seq[Double](0.249147045813403,
    0.233492536538355,
    0.203167426723066,
    0.160078328543346,
    0.106939325995318,
    0.047175336386512)

  private val n12 = 6

  /* order 20 */
  private val x20 = Seq[Double](0.076526521133497,
    0.227785851141645,
    0.373706088715420,
    0.510867001950827,
    0.636053680726515,
    0.746331906460151,
    0.839116971822219,
    0.912234428251326,
    0.963971927277914,
    0.993128599185095)

  private  val w20 = Seq[Double](0.152753387130726,
    0.149172986472604,
    0.142096109318382,
    0.131688638449177,
    0.118194531961518,
    0.101930119817240,
    0.083276741576704,
    0.062672048334109,
    0.040601429800387,
    0.017614007139152)

  private val n20 = 10
}
