package org.quantlib.math.solver1d

import org.quantlib.math.solver1d.Solver1D.SolverIDImPl
import org.quantlib.math.Comparing._

/**
  * Created by neo on 08/04/2017.
  */
final case class Secant(lowerBound: Double = 0.0,
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
    var rootLocal = rootExternal
    val (xMinLocal, xMaxLocal) = (xMinExternal, xMaxExternal)
    val (fxMinLocal, fxMaxLocal) = (fxMinExternal, fxMaxExternal)
    var froot = 0.0
    var xl = 0.0
    var fl = 0.0

    if (Math.abs(fxMinLocal) < Math.abs(fxMaxLocal)) {
      rootLocal = xMinLocal
      froot = fxMinLocal
      xl = xMaxLocal
      fl = fxMaxLocal
    } else {
      rootLocal = xMaxLocal
      froot = fxMaxLocal
      xl = xMinLocal
      fl = fxMinLocal
    }

    var loopingResult = 0.0
    var resultReached = false
    var evaluationNumber = evalCount + 1

    while (!resultReached && evaluationNumber <= maxEvaluations) {

      val dx = (xl - rootLocal) * froot / (froot - fl)
      xl = rootLocal
      fl = froot
      rootLocal += dx
      froot = f(rootLocal)
      evaluationNumber += 1
      if (Math.abs(dx) < xAccuracy || froot =~ 0.0) {
        loopingResult = rootLocal
        resultReached = true
      }
    }
    assert(evaluationNumber <= maxEvaluations,
      s"unable to bracket root in ${maxEvaluations} function evaluations")

    loopingResult
  }
}
