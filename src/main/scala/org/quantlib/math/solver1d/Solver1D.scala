package org.quantlib.math.solver1d

import org.quantlib.math.Constants
import org.quantlib.math.Comparing._
/**
  * Created by neo on 11/17/15.
  */




object Solver1D {

  trait SolverIDImPl {
    def maxEvaluations: Int

    def lowerBound: Double

    def upperBound: Double

    def lowerBoundEnforced: Boolean

    def upperBoundEnforced: Boolean

   // def solve(f: (Double => Double), xAccuracy: Double): Double
    def solveImpl(f: (Double) => Double,
      root: Double,
      xMin: Double, xMax: Double,
      fxMin: Double, fxMax: Double,
      evalCount: Int,
      xAccuracy: Double): Double
  }

  implicit class Solver1DOps(val impl: SolverIDImPl) extends AnyVal {

    private def check(accuracy: Double) = require(accuracy > 0.0, s"accuracy ($accuracy) must be positive")

    def solve(f: (Double => Double), accuracy: Double, guess: Double, step: Double) = {

      var (xMin, xMax) = (0.0, 0.0)
      var (fxMin, fxMax) = (0.0, 0.0)
      var root = 0.0
      var evaluationNumber = 2

      def enforceBounds(x: Double): Double = {
        if (impl.lowerBoundEnforced && x < impl.lowerBound) impl.lowerBound
        else if (impl.upperBoundEnforced && x > impl.upperBound) impl.upperBound
        else x
      }

      check(accuracy)

      val accuracyPromximation = Math.max(accuracy, Constants.QL_EPSILON)

      var growthFactor = 1.6
      var flipflop = -1

      root = guess
      fxMin = f(root)

      // monotonically crescent bias, as in optionValue(volatility)

      val result =
        if (fxMax =~ 0.0) {
          root
        } else {

          if (fxMax > 0.0) {
            xMin = enforceBounds(root - step)
            fxMin = f(xMin)
            xMax = root
          } else {
            xMin = root
            fxMin = fxMax
            xMax = enforceBounds(root + step)
            fxMax = f(xMax)
          }


          var loopingResult = 0.0
          var resultReached = false
          while (!resultReached || evaluationNumber <= impl.maxEvaluations) {
            if (fxMin * fxMax <= 0.0) {
              loopingResult =
                if (fxMin =~ 0.0) {
                  xMin
                } else if (fxMax =~ 0.0) {
                  xMax
                } else {
                  root = (xMax + xMin) / 2.0
                  impl.solveImpl(f, root,
                    xMin, xMax,
                    fxMin, fxMax,
                    evaluationNumber,
                    accuracyPromximation)
                }
              resultReached = true //terminating the while loop
            } else {

              if (Math.abs(fxMin) < Math.abs(fxMax)) {
                xMin = enforceBounds(xMin + growthFactor * (xMin - xMax))
                fxMin = f(xMin)
              } else if (Math.abs(fxMin) > Math.abs(fxMax)) {
                xMax = enforceBounds(xMax + growthFactor * (xMax - xMin))
                fxMax = f(xMax)
              } else if (flipflop == -1) {
                xMin = enforceBounds(xMin + growthFactor * (xMin - xMax))
                fxMin = f(xMin)
                evaluationNumber = evaluationNumber + 1
                flipflop = 1
              } else if (flipflop == 1) {
                xMax = enforceBounds(xMax + growthFactor * (xMax - xMin))
                fxMax = f(xMax)
                flipflop = -1
              }

              evaluationNumber = evaluationNumber + 1
            }
          }

          assert(evaluationNumber > impl.maxEvaluations,
            s"unable to bracket root in ${impl.maxEvaluations} function evaluations " +
              s"(last bracket attempt: f[$xMin , $xMax] -> [$fxMin , $fxMax])")

          loopingResult

        }

      result
    }

    def solve(f: (Double => Double),
              accuracy: Double,
              guess: Double,
              min: Double,
              max: Double): Double = {
      check(accuracy)
      require(min < max, s"invalid range: xMin ($min) >= xMax ($max)")
      require(!impl.lowerBoundEnforced || min >= impl.lowerBound, s"xMin ($min) < enforced low bound ($impl.lowerBound)")
      require(!impl.upperBoundEnforced || max <= impl.upperBound, s"xMax ($max) > enforced hi bound ($impl.upperBound)")

      val xMax = max
      val xMin = min
      val fxMax = f(xMax)
      val fxMin = f(xMin)

      val accuracyPromximation = Math.max(accuracy, Constants.QL_EPSILON)

      val result =
        if (fxMin =~ 0.0) {
          xMin
        } else if (fxMax =~ 0.0) {
          xMax
        } else {

          require(fxMin * fxMax < 0.0, s"root not bracketed: f[$xMin , $xMax] -> [$fxMin , $fxMax]")
          require(guess > xMin, s"guess ($guess) < xMin ($xMin)")
          require(guess < xMax, s"guess ($guess) > xMax ($xMax)")


          impl.solveImpl(f, guess,
            xMin, xMax,
            fxMin, fxMax,
            0,
            accuracyPromximation)
        }
      result
    }

  }

}
