package org.quantlib.math.solver1d

import org.quantlib.math.solver1d.Solver1D.SolverIDImPl
import org.quantlib.math.Comparing._

/**
  * Created by neo on 08/04/2017.
  */
final case class FalsePosition(lowerBound: Double = 0.0,
                     upperBound: Double = 0.0,
                     maxEvaluations: Int = 100,
                     lowerBoundEnforced: Boolean = false,
                     upperBoundEnforced: Boolean = false) extends SolverIDImPl {

  override def solveImpl(f: Solver1D.StatFunction,
                         rootExternal: Double,
                         xMinExternal: Double, xMaxExternal: Double,
                         fxMinExternal: Double, fxMaxExternal: Double,
                         evalCount: Int,
                         xAccuracy: Double): Double = {

    // Identify the limits so that xl corresponds to the low side
    var (fl, fh, xl, xh) =  if (fxMinExternal < 0.0) {
      (fxMinExternal, fxMaxExternal, xMinExternal, xMaxExternal)
    } else {
      (fxMaxExternal, fxMinExternal, xMaxExternal, xMinExternal)
    }


    var froot = f(rootExternal)

    var evaluationNumber = evalCount + 1

    var loopingResult = 0.0
    var resultReached = false
    var rootLocal = rootExternal

    var del = 0.0
    while (!resultReached && evaluationNumber <= maxEvaluations) {
      // Increment with respect to latest value
      rootLocal = xl+(xh-xl)*fl/(fl-fh)
      froot = f(rootLocal)
      evaluationNumber += 1
      if (froot < 0.0) {       // Replace appropriate limit
        del = xl-rootLocal
        xl = rootLocal
        fl = froot
      } else {
        del = xh-rootLocal
        xh = rootLocal
        fh = froot
      }
      // Convergence criterion
      if (Math.abs(del) < xAccuracy || froot =~ 0.0) {

        loopingResult =  rootLocal
        resultReached = true
      }
    }

    assert(evaluationNumber <= maxEvaluations,
      s"maximum number of function evaluations ($maxEvaluations) exceeded")

    loopingResult
  }
  
}
