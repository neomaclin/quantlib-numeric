package org.quantlib.math.functions

import org.quantlib.math.Complex
import org.scalatest.FunSuite

/**
  * Created by neo on 09/04/2017.
  */
class FFTSuite extends FunSuite {
  test("Testing complex direct FFT...") {
    val a:Seq[Complex] = Seq(
      Complex(0,0), Complex(1,1),
      Complex(3,3), Complex(4,4),
      Complex(4,4), Complex(3,3),
      Complex(1,1), Complex(0,0)
    )

    val fft = FFT(3)
    val b = fft.transform(a.toList)
    val expecteds: Seq[Complex] = Seq(
      Complex(16,16), Complex(-4.8284,-11.6569),
      Complex(0,0),   Complex(-0.3431,0.8284),
      Complex(0,0),   Complex(0.8284, -0.3431),
      Complex(0,0),   Complex(-11.6569,-4.8284)
    )

    b zip expecteds foreach { case (calculated, expected ) =>
        assert(Math.abs(calculated.real - expected.real) <= 1.0e-2 && Math.abs(calculated.imag - expected.imag) <= 1.0e-2)

    }
  }

  test("Testing inverse FFT...") {
    val x = Seq(1,2,3,4).map(Complex(_))

    val fft = FFT(2)
    val nFrq = fft.outputSize
    val ft = fft.transform(x)
    val ift = fft.transform(ft, inverse = true)

    x zip ift foreach{ case( Complex(expected,_), Complex(calculated,_) ) =>
      assert(expected == calculated/nFrq)
    }


  }
}
