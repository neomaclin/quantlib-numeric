package org.quantlib.math

/**
  * Created by neo on 23/03/2017.
  */
object PascalTriangle {

  private lazy val coefficients: Stream[Seq[BigInt]] = Stream.iterate(Seq(BigInt(1))){ line =>
    (BigInt(0)+:line, line:+BigInt(0)).zipped.map{ (a,b) => a + b }
  }

  def get(order: Int): Seq[BigInt] = coefficients(order)


}
