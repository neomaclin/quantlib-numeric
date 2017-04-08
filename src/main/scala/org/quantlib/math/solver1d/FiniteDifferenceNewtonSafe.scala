package org.quantlib.math.solver1d

import org.quantlib.math.solver1d.Solver1D.SolverIDImPl
import org.quantlib.math.Comparing._

/**
  * Created by neo on 08/04/2017.
  */
final case class FiniteDifferenceNewtonSafe(lowerBound: Double = 0.0,
                                            upperBound: Double = 0.0,
                                            maxEvaluations: Int = 100,
                                            lowerBoundEnforced: Boolean = false,
                                            upperBoundEnforced: Boolean = false) extends SolverIDImPl {
  if (lowerBound > 0.0) require(lowerBoundEnforced)
  if (upperBound > 0.0) require(upperBoundEnforced)

  override def solveImpl(f: Solver1D.StatFunction,
                         rootExternal: Double,
                         xMinExternal: Double, xMaxExternal: Double,
                         fxMinExternal: Double, fxMaxExternal: Double,
                         evalCount: Int,
                         xAccuracy: Double): Double = {
    var (xh, xl) =
      if (fxMinExternal < 0.0) {
        (xMaxExternal, xMinExternal)

      } else {
        (xMinExternal, xMaxExternal)

      }

    var froot = f(rootExternal)

    // first order finite difference derivative
    var dfroot = if (xMaxExternal - rootExternal < rootExternal - xMinExternal) {
      (fxMaxExternal - froot) / (xMaxExternal - rootExternal)
    } else {
      (fxMinExternal - froot) / (xMinExternal - rootExternal)
    }

    var rootLocal = rootExternal

    // xMax_-xMin_>0 is verified in the constructor
    var dx = xMaxExternal - xMinExternal
    var evaluationNumber = evalCount + 1

    var loopingResult = 0.0
    var resultReached = false

    var frootold = froot
    var rootold = rootLocal
    var dxold = dx

    while (!resultReached && evaluationNumber <= maxEvaluations) {
      frootold = froot
      rootold = rootLocal
      dxold = dx
      // Bisect if (out of range || not decreasing fast enough)
      if ((((rootLocal - xh) * dfroot - froot) * ((rootLocal - xl) * dfroot - froot) > 0.0) ||
              (Math.abs(2.0 * froot) > Math.abs(dxold * dfroot))) {
        dx = (xh - xl) / 2.0
        rootLocal = xl + dx
        // if the root estimate just computed is close to the
        // previous one, we should calculate dfroot at root and
        // xh rather than root and rootold (xl instead of xh would
        // be just as good)
        if (rootLocal.=~(rootold)(2500, DoubleProximity)) {
          rootold = xh
          frootold = f(xh)
        }
      } else { // Newton
        dx = froot / dfroot
        rootLocal -= dx
      }

      // Convergence criterion
      if (Math.abs(dx) < xAccuracy) {
        loopingResult =  rootLocal
        resultReached = true
      } else {
        froot = f(rootLocal)
        evaluationNumber = evaluationNumber + 1
        dfroot = (frootold - froot) / (rootold - rootLocal)

        if (froot < 0.0)
          xl = rootLocal
        else
          xh = rootLocal
      }
    }
    assert(evaluationNumber <= maxEvaluations,
      s"maximum number of function evaluations ($maxEvaluations) exceeded")

    loopingResult
  }
}