package org.quantlib.math.functions

import org.quantlib.math.Complex

/**
  * Created by neo on 23/03/2017.
  */
trait ModifiedBessel {

  def modifiedBesselFunction_i(nu: Double, x: Double): Double = ???
  def modifiedBesselFunction_k(nu: Double, x: Double): Double = ???
  def modifiedBesselFunction_i_exponentiallyWeighted(nu: Double, x:Double): Double = ???
  def modifiedBesselFunction_k_exponentiallyWeighted(nu: Double, x:Double): Double = ???

  def modifiedBesselFunction_i(nu: Double, z: Complex): Complex = ???
  def modifiedBesselFunction_k(nu: Double, z: Complex): Complex = ???
  def modifiedBesselFunction_i_exponentiallyWeighted(nu: Double, z: Complex): Complex = ???
  def modifiedBesselFunction_k_exponentiallyWeighted(nu: Double, z: Complex): Complex = ???

}
