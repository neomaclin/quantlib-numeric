package org.quantlib.math

/**
  * Created by neo on 04/04/2017.
  */
object Utilities {
  def norm(seq: Seq[Double], h: Double): Double =  {
    // squared values

    val f2 = seq.map( x=> x * x )
    // numeric integral of f^2
    val head = f2.head
    val last = f2.last
    val I = h * (f2.fold(0.0)(_+_) - 0.5*head - 0.5*last)

    Math.sqrt(I)
  }

}
