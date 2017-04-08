package org.quantlib.math.solver1d


import org.quantlib.math.solver1d.Solver1D.SolverIDImPl
import org.quantlib.math.Constants._
import org.quantlib.math.Comparing._

/**
  * Created by neo on 06/03/2017.
  */
final case class Brent(lowerBound: Double = 0.0,
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

    //var min1, min2
    // var froot, p, q, r, s, xAcc1, xMid
    var rootLocal = rootExternal
    var (xMinLocal, xMaxLocal) = (xMinExternal, xMaxExternal)
    var (fxMinLocal, fxMaxLocal) = (fxMinExternal, fxMaxExternal)
    // we want to start with root (which equals the guess) on
    // one side of the bracket and both xMin and xMax on the
    // other.
    var froot = f(rootLocal)
    var evaluationNumber = evalCount + 1
    var (p, q, r, s, xAcc1, xMid) = (0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
    if ((froot * fxMinLocal) < 0) {
      xMaxLocal = xMinLocal
      fxMaxLocal = fxMinLocal
    } else {
      xMinLocal = xMaxLocal
      fxMinLocal = fxMaxLocal
    }


    var d = rootLocal - xMaxLocal
    var e = d
    var loopingResult = 0.0
    var resultReached = false
    while (!resultReached && evaluationNumber <= maxEvaluations) {
      if ((froot > 0.0 && fxMaxLocal > 0.0) || (froot < 0.0 && fxMaxLocal < 0.0)) {

        // Rename xMin, root, xMax and adjust bounds
        xMaxLocal = xMinLocal
        fxMaxLocal = fxMinLocal
        d = rootLocal - xMinLocal
        e = d
      }
      if (Math.abs(fxMaxLocal) < Math.abs(froot)) {
        xMinLocal = rootLocal
        rootLocal = xMaxLocal
        xMaxLocal = xMinLocal
        fxMinLocal = froot
        froot = fxMaxLocal
        fxMaxLocal = fxMinLocal
      }
      // Convergence check
      xAcc1 = 2.0 * QL_EPSILON * Math.abs(rootLocal) + 0.5 * xAccuracy
      xMid = (xMaxLocal - rootLocal) / 2.0
      if (Math.abs(xMid) <= xAcc1 || froot =~ 0.0) {
        //froot = f(rootLocal)
        evaluationNumber = evaluationNumber + 1
        loopingResult = rootLocal
        resultReached = true
      } else {
        if (Math.abs(e) >= xAcc1 && Math.abs(fxMinLocal) > Math.abs(froot)) {

          // Attempt inverse quadratic interpolation
          s = froot / fxMinLocal
          if (xMinLocal =~ xMaxLocal) {
            p = 2.0 * xMid * s
            q = 1.0 - s
          } else {
            q = fxMinLocal / fxMaxLocal
            r = froot / fxMaxLocal
            p = s * (2.0 * xMid * q * (q - r) - (rootLocal - xMinLocal) * (r - 1.0))
            q = (q - 1.0) * (r - 1.0) * (s - 1.0)
          }
          if (p > 0.0) q = -q // Check whether in bounds
          p = Math.abs(p)
          val min1 = 3.0 * xMid * q - Math.abs(xAcc1 * q)
          val min2 = Math.abs(e * q)
          if (2.0 * p < (if (min1 < min2) min1 else min2)) {
            e = d // Accept interpolation
            d = p / q
          } else {
            d = xMid // Interpolation failed, use bisection
            e = d
          }
        } else {
          // Bounds decreasing too slowly, use bisection
          d = xMid
          e = d
        }
        xMinLocal = rootLocal
        fxMinLocal = froot
        rootLocal = if (Math.abs(d) > xAcc1) rootLocal + d else rootLocal + Solver1D.sign(xAcc1, xMid)
        froot = f(rootLocal)
        evaluationNumber = evaluationNumber + 1

      }
    }
    assert(evaluationNumber <= maxEvaluations,
      s"unable to bracket root in ${maxEvaluations} function evaluations " +
        s"(last bracket attempt: f[$xMinLocal , $xMaxLocal] -> [$fxMinLocal , $fxMaxLocal])")

    loopingResult

  }
}
