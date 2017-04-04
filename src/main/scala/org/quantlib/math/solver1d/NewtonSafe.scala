//package org.quantlib.org.quantlib.math.solver1d
//
//import org.quantlib.org.quantlib.math.functions.StatisticFunction
//
///**
//  * Created by neo on 11/20/15.
//  */
//case class NewtonSafe( maxEvaluations: Int,
//                       lowerBound: Double,
//                       upperBound: Double,
//                       lowerBoundEnforced: Boolean = false,
//                       upperBoundEnforced: Boolean = false)
//  extends Solver1D[NewtonSafe]{
//
//  def solve(f: StatisticFunction, xAccuracy: Double): Double = {
//    var (froot, dfroot, dx, dxold) = (0.0,0.0,0.0,0.0)
//    var (xh, xl) = (0.0, 0.0)
//
//    // Orient the search so that f(xl) < 0
//    if (fxMin < 0.0) {
//      xl = xMin
//      xh = xMax
//    } else {
//      xh = xMin
//      xl = xMax
//    }
//
//    // the "stepsize before last"
//    dxold = xMax-xMin
//    // it was dxold=std::fabs(xMax-xMin) in Numerical Recipes
//    // here (xMax-xMin > 0) is verified in the constructor
//
//    // and the last step
//    dx = dxold
//
//    froot = f(root)
//    dfroot = f.derivative(root)
//   // require(dfroot != Null<Real>(),
//   //   "NewtonSafe requires function's derivative")
//    evaluationNumber = evaluationNumber + 1
//
//    while (evaluationNumber<=maxEvaluations) {
//      // Bisect if (out of range || not decreasing fast enough)
//      if ((((root-xh)*dfroot-froot)*
//        ((root-xl)*dfroot-froot) > 0.0)
//        || (Math.abs(2.0*froot) > Math.abs(dxold*dfroot))) {
//
//        dxold = dx
//        dx = (xh-xl)/2.0
//        root=xl+dx
//      } else {
//        dxold = dx
//        dx = froot/dfroot
//        root -= dx
//      }
//      // Convergence criterion
//      if (Math.abs(dx) < xAccuracy) {
//        f(root)
//        evaluationNumber = evaluationNumber + 1
//        return root
//      }
//      froot = f(root)
//      dfroot = f.derivative(root)
//         evaluationNumber = evaluationNumber + 1
//      if (froot < 0.0)
//        xl=root
//      else
//        xh=root
//    }
//
//    assert(evaluationNumber > maxEvaluations,
//      s"maximum number of function evaluations ($maxEvaluations) exceeded")
//  }
//}
