package org.quantlib.math.solver1d

import org.quantlib.math.functions.StatisticFunction
import org.quantlib.math.solver1d.Solver1D.SolverIDImPl
import org.quantlib.math.Constants._
import org.quantlib.math.Comparing._

/**
  * Created by Neo on 2016/7/30.
  */

final case class Bisection(lowerBound: Double = 0.0,
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

    var (dx, xMid, fMid) = (0.0, 0.0, 0.0)
    var rootLocal = 0.0
    // Orient the search so that f>0 lies at root_+dx
    if (fxMinExternal < 0.0) {
      dx = xMaxExternal - xMinExternal
      rootLocal = xMinExternal
    } else {
      dx = xMinExternal-xMaxExternal
      rootLocal = xMaxExternal
    }

    var evaluationNumber = evalCount
    var loopingResult = 0.0
    var resultReached = false

    while (!resultReached && evaluationNumber <= maxEvaluations) {
      dx /= 2.0
      xMid = rootLocal + dx
      fMid = f(xMid)
      evaluationNumber += 1
      if (fMid <= 0.0)
        rootLocal = xMid
      if (Math.abs(dx) < xAccuracy || fMid =~ 0.0 ) {
       // f(root_); should we keep this for probing test?
        evaluationNumber += 1
        loopingResult =  rootLocal
        resultReached = true
      }
    }
    assert(evaluationNumber <= maxEvaluations,
      s"unable to bracket root in ${maxEvaluations} function evaluations ")

    loopingResult
  }
}
