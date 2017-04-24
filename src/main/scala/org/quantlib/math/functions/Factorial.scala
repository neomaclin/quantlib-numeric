package org.quantlib.math.functions

object Factorial {

  private val N: Stream[Double] = 1.0 #:: N.map(_ + 1.0)
  private val fibs: Stream[Double] =  1.0 #:: fibs.zip(N).map { case (x,y) => x * y }

  def get(n: Int): Double = fibs(n)
  def ln(n: Int): Double = Math.log(fibs(n))

}
