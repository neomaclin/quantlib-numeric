package org.quantlib.math

import org.quantlib.math.Rounding.Type._

sealed abstract class Rounding(roundingType: Rounding.Type) extends (Double => Double){

  def precision: Int
  def digit: Int

  def apply(oldValue: Double): Double = {
    val multiplier = Math.pow(10.0, precision)
    val isNegative = oldValue < 0.0
    val largeValue = Math.abs(oldValue) * multiplier
    val integral = largeValue.toLong.toDouble
    val fractional = largeValue - integral

    def roundUp = (integral + 1.0) / multiplier
    def roundDown = integral / multiplier
    def roundToClosest = if (fractional >= (digit / 10.0)) roundUp else roundDown

    val newValue = roundingType match {
      case Up => if (fractional != 0.0) roundUp else integral / multiplier
      case Down => roundDown
      case Closest => roundToClosest
      case Floor => if (!isNegative) roundToClosest else roundDown
      case Ceiling => if (isNegative) roundToClosest else roundDown
      case No => Math.abs(oldValue)
    }
    if (isNegative) -newValue else newValue
  }

}

object Rounding{

  def apply(precision: Int = 0, digit: Int = 5): Rounding = {
    ClosestRounding(precision, digit)
  }

  final case class NoRounding(precision: Int = 0, digit: Int = 5) extends Rounding(No)

  final case class DownRounding(precision: Int, digit: Int = 5) extends Rounding(Down)

  final case class UpRounding(precision: Int, digit: Int = 5) extends Rounding(Up)

  final case class ClosestRounding(precision: Int, digit: Int = 5) extends Rounding(Closest)

  final case class CeilingRounding(precision: Int, digit: Int = 5) extends Rounding(Ceiling)

  final case class FloorRounding(precision: Int, digit: Int = 5) extends Rounding(Floor)

  sealed trait Type

  object Type {

    case object No extends Type
    case object Up extends Type
    case object Down extends Type
    case object Closest extends Type
    case object Floor extends Type
    case object Ceiling extends Type

  }

}

