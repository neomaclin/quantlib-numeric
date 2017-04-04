package org.quantlib.math.randomnumbers

/**
  * Created by neo on 23/03/2017.
  */
trait RNG[A] {

}

final case class Sample[T](value :T, weight: Double)