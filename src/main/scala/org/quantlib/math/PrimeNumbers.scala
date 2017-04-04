package org.quantlib.math

/**
  * Created by neo on 23/03/2017.
  */
object PrimeNumbers {
  lazy val ps: Stream[BigInt] = BigInt(2) #:: Stream.from(3).map(x => BigInt(x)).filter{ i  =>
    ps.takeWhile{ p  => p * p <= i}.forall{ p => i % p > 0}
  }

  def get(indx: Int): BigInt = ps(indx)

}
