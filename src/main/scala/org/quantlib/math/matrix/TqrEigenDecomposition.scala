package org.quantlib.math.matrix

import org.quantlib.math.matrix.TqrEigenDecomposition.EigenVectorCalculation._
import org.quantlib.math.matrix.TqrEigenDecomposition.ShiftStrategy._
import org.quantlib.math.matrix.TqrEigenDecomposition._

/**
  * Created by neo on 05/03/2017.
  */
final case class TqrEigenDecomposition(diag: Seq[Double],
                                       sub: Seq[Double],
                                       calc: EigenVectorCalculation = WithEigenVector,
                                       strategy: ShiftStrategy = CloseEigenValue) {
  require(diag.length == sub.length + 1, "Wrong dimensions")
  private var iter = 0
  private val n = diag.length
  private val e: Seq[Double] = 0.0 +: sub

  private def offDiagIsZero(k: Int, e: Seq[Double]): Boolean = {
    val left: Double = Math.abs(diag(k - 1)) + Math.abs(diag(k))
    val right: Double = Math.abs(diag(k - 1)) + Math.abs(diag(k)) + Math.abs(e(k))
    left == right
  }

  val eigenvalues: Seq[Double] = e
  val eigenvectors: Seq[Seq[Double]] = ???

  def iterations: Int = iter


}

object TqrEigenDecomposition {

  sealed trait EigenVectorCalculation

  object EigenVectorCalculation {

    case object WithEigenVector extends EigenVectorCalculation

    case object WithoutEigenVector extends EigenVectorCalculation

    case object OnlyFirstRowEigenVector extends EigenVectorCalculation

  }


  sealed trait ShiftStrategy

  object ShiftStrategy {

    case object NoShift extends ShiftStrategy

    case object Overrelaxation extends ShiftStrategy

    case object CloseEigenValue extends ShiftStrategy

  }


}