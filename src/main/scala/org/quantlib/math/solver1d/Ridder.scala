package org.quantlib.math.solver1d

import org.quantlib.math.solver1d.Solver1D._
import org.quantlib.math.Comparing._

/**
  * Created by neo on 08/04/2017.
  */
final case class Ridder(lowerBound: Double = 0.0,
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

    var evaluationNumber = evalCount + 1

    var fxMid = 0.0
    var froot = 0.0
    var s = 0.0
    var xMid = 0.0
    var nextRoot = 0.0

    var rootLocal = rootExternal
    var (xMinLocal, xMaxLocal) = (xMinExternal, xMaxExternal)
    var (fxMinLocal, fxMaxLocal) = (fxMinExternal, fxMaxExternal)

    // test on Black-Scholes implied volatility show that
    // Ridder solver algorithm actually provides an
    // accuracy 100 times below promised
    val xAccuracyLocal = xAccuracy / 100.0


    var loopingResult = 0.0
    var resultReached = false
    while (!resultReached && evaluationNumber <= maxEvaluations) {
      xMid = 0.5 * (xMinLocal + xMaxLocal)
      // First of two function evaluations per iteraton
      fxMid = f(xMid)
      evaluationNumber += 1
      s = Math.sqrt(fxMid * fxMid - fxMinLocal * fxMaxLocal)
      if (s =~ 0.0) {
        //f(rootLocal)
        evaluationNumber += 1
        loopingResult =  rootLocal
        resultReached = true
      } else {
        // Updating formula
        nextRoot = xMid + (xMid - xMinLocal) * ((if (fxMinLocal >= fxMaxLocal) 1.0 else -1.0) * fxMid / s)
        if (Math.abs(nextRoot - rootLocal) <= xAccuracyLocal) {
         // f(rootLocal)
          evaluationNumber += 1
          loopingResult =  rootLocal
          resultReached = true
        } else {

          rootLocal = nextRoot
          // Second of two function evaluations per iteration
          froot = f(rootLocal)
          evaluationNumber += 1
          if (froot =~ 0.0) {
            loopingResult =  rootLocal
            resultReached = true
          } else {


            // Bookkeeping to keep the root bracketed on next iteration
            if (sign(fxMid, froot) != fxMid) {
              xMinLocal = xMid
              fxMinLocal = fxMid
              xMaxLocal = rootLocal
              fxMaxLocal = froot
            } else if (sign(fxMinLocal, froot) != fxMinLocal) {
              xMaxLocal = rootLocal
              fxMaxLocal = froot
            } else if (sign(fxMaxLocal, froot) != fxMaxLocal) {
              xMinLocal = rootLocal
              fxMinLocal = froot
            } else {

            }

            if (Math.abs(xMaxLocal - xMinLocal) <= xAccuracyLocal) {
              //f(rootLocal)
              evaluationNumber += 1
              loopingResult = rootLocal
              resultReached = true
            }
          }
        }
      }
    }

    assert(evaluationNumber <= maxEvaluations,
      s"unable to bracket root in ${maxEvaluations} function evaluations")

    loopingResult

  }

}