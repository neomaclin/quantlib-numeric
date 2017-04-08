package org.quantlib.math.solver1d

import org.quantlib.math.solver1d.Solver1D.SolverIDImPl
import org.quantlib.math.solver1d.Solver1D._

/**
  * Created by neo on 08/04/2017.
  */

final case class Newton(lowerBound: Double = 0.0,
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



    var froot = f(rootExternal)
    var dfroot = f.derivative(rootExternal)
    var evaluationNumber = evalCount + 1

    var loopingResult = 0.0
    var resultReached = false
    var rootLocal = rootExternal

    var dx = 0.0
    while (!resultReached && evaluationNumber <= maxEvaluations) {
      dx = froot/dfroot;
      rootLocal -= dx
      // jumped out of brackets, switch to NewtonSafe
      if ((xMinExternal-rootLocal)*(rootLocal-xMaxExternal) < 0.0) {
        val s = NewtonSafe(maxEvaluations = this.maxEvaluations - evaluationNumber)
        loopingResult =  s.solve(f, xAccuracy, rootLocal+dx, xMinExternal, xMaxExternal)
        resultReached = true
      }
      if (Math.abs(dx) < xAccuracy) {
       // f(root_);
        evaluationNumber = evaluationNumber + 1
        loopingResult =  rootLocal
        resultReached = true
      }
      froot = f(rootLocal)
      dfroot = f.derivative(rootLocal)
      evaluationNumber = evaluationNumber + 1
    }

    assert(evaluationNumber <= maxEvaluations,
      s"maximum number of function evaluations ($maxEvaluations) exceeded")

    loopingResult
  }
}