package org.quantlib.math.functions

import org.nd4j.linalg.api.complex.{IComplexNDArray, IComplexNumber}
import org.nd4j.linalg.cpu.nativecpu.NDArray
import org.nd4j.linalg.fft.FFTInstance
import org.quantlib.math.Complex
import org.quantlib.math.Complex._
import org.quantlib.math.Constants._

import scala.annotation.tailrec
import org.nd4s.Implicits._

import scala.collection.mutable

/**
  * Created by neo on 23/03/2017.
  */
object FFT{

  def minOrder(inputSize: Int): Int = Math.ceil(Math.log(inputSize) / M_LN2).toInt

}

final case class FFT(inputSize: Int) {

  def outputSize: Int = 1 << inputSize

  private lazy val cs: Stream[Double] =
    Math.cos(2 * M_PI / Math.pow(2, inputSize)) #:: (cs zip sn).map { case (c, s) => c * c - s * s }

  private lazy val sn: Stream[Double] =
    Math.sin(2 * M_PI / Math.pow(2, inputSize)) #:: (cs zip sn).map { case (c, s) => 2 * s * c }


  @tailrec
  private def reverse(in: Int, i: Int = 0, n: Int = 0): Int = {
    if (i == inputSize) n
    else reverse(in >> 1, i + 1, (n << 1) | (in & 1))
  }

  private def copy(in: Seq[Complex]): Array[Complex] = {
    val result: Array[Complex] = Array.ofDim[Complex](Math.pow(2, inputSize).toInt).map(_=>Complex(0.0))
    in.indices foreach { k => result(reverse(k)) = in(k) }
    result
  }

  def transform(in: Seq[Complex], inverse: Boolean = false): Seq[Complex] = {
    require(Math.pow(2, inputSize).toInt >= in.length, "FFT order is too small")
    val order = inputSize
    val N = in.length
    val X = copy(in)
    val cos = cs.take(inputSize).reverse
    val sin = sn.take(inputSize).reverse

    1 to order foreach { s =>
      val m = Math.pow(2, s).toInt
      val wm = Complex(cos(s - 1), if (inverse) sin(s - 1) else -sin(s - 1))
      var w = Complex(1.0)
      0 until (m / 2) foreach { j: Int =>
        j until (N, m) foreach { k: Int =>
          val t = w * X(k + m / 2)
          val u = X(k)
          X(k) = u + t
          X(k + m / 2) = u - t
        }
        w = w * wm
      }
    }
    X

  }
}
