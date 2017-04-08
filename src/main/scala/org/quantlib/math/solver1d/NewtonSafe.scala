package org.quantlib.math.solver1d


import org.quantlib.math.solver1d.Solver1D.SolverIDImPl

/**
  * Created by neo on 11/20/15.
  */
import scala.languageFeature.reflectiveCalls

final case class NewtonSafe(lowerBound: Double = 0.0,
                               upperBound: Double = 0.0,
                               maxEvaluations: Int = 100,
                               lowerBoundEnforced: Boolean = false,
                               upperBoundEnforced: Boolean = false) extends SolverIDImPl {
  if (lowerBound>0.0)require(lowerBoundEnforced)
  if (upperBound>0.0)require(upperBoundEnforced)
  override def solveImpl(f: Solver1D.StatFunction,
                         rootExternal: Double,
                         xMinExternal: Double, xMaxExternal: Double,
                         fxMinExternal: Double, fxMaxExternal: Double,
                         evalCount: Int,
                         xAccuracy: Double): Double = {

    var (froot, dfroot, dx, dxold) = (0.0,0.0,0.0,0.0)
    var (xh, xl) = (0.0, 0.0)

    // Orient the search so that f(xl) < 0
    if (fxMinExternal < 0.0) {
      xl = xMinExternal
      xh = xMaxExternal
    } else {
      xh = xMinExternal
      xl = xMaxExternal
    }

    // the "stepsize before last"
    dxold = xMaxExternal-xMinExternal
    // it was dxold=std::fabs(xMax-xMin) in Numerical Recipes
    // here (xMax-xMin > 0) is verified in the constructor

    // and the last step
    dx = dxold

    froot = f(rootExternal)
    dfroot = f.derivative(rootExternal)
    var rootLocal = rootExternal
   // require(dfroot != Null<Real>(),
   //   "NewtonSafe requires function's derivative")
    var evaluationNumber = evalCount + 1

    var loopingResult = 0.0
    var resultReached = false

    while (!resultReached && evaluationNumber <= maxEvaluations) {
      // Bisect if (out of range || not decreasing fast enough)
      if ((((rootLocal-xh)*dfroot-froot)* ((rootLocal-xl)*dfroot-froot) > 0.0) ||
        (Math.abs(2.0*froot) > Math.abs(dxold*dfroot))) {

        dxold = dx
        dx = (xh-xl)/2.0
        rootLocal=xl+dx
      } else {
        dxold = dx
        dx = froot/dfroot
        rootLocal -= dx
      }
      // Convergence criterion
      if (Math.abs(dx) < xAccuracy) {
        //f(root)
        evaluationNumber = evaluationNumber + 1
        loopingResult =  rootLocal
        resultReached = true
      }
      froot = f(rootLocal)
      dfroot = f.derivative(rootLocal)
      evaluationNumber = evaluationNumber + 1
      if (froot < 0.0) xl=rootLocal
      else xh=rootLocal
    }

    assert(evaluationNumber <= maxEvaluations,
      s"maximum number of function evaluations ($maxEvaluations) exceeded")

    loopingResult
  }
}
