package org.quantlib.math

import java.lang.Math._

import org.nd4j.linalg.api.complex.IComplexNumber
import org.nd4j.linalg.factory.Nd4j

import scala.collection.generic.CanBuildFrom
/**
  * Created by neo on 23/03/2017.
  */
final case class Complex(real : Double, imag : Double = 0.0) {
  override val toString = real + " + " + imag + "i"


  def +(that : Complex) =
    Complex(this.real + that.real, this.imag + that.imag)

  def +(that : Int) =
    Complex(this.real + that, this.imag)

  def +(that : Long) =
    Complex(this.real + that, this.imag)

  def +(that : Float) =
    Complex(this.real + that, this.imag)

  def +(that : Double) =
    Complex(this.real + that, this.imag)

  def -(that : Complex) =
    Complex(this.real - that.real, this.imag - that.imag)

  def -(that : Int) =
    Complex(this.real - that, this.imag)

  def -(that : Long) =
    Complex(this.real - that, this.imag)

  def -(that : Float) =
    Complex(this.real - that, this.imag)

  def -(that : Double) =
    Complex(this.real - that, this.imag)

  def *(that : Complex) =
    Complex(this.real * that.real - this.imag * that.imag,
      this.real * that.imag + this.imag * that.real)

  def *(that : Int) =
    Complex(this.real * that, this.imag * that)

  def *(that : Long) =
    Complex(this.real * that, this.imag * that)

  def *(that : Float) =
    Complex(this.real * that, this.imag * that)

  def *(that : Double) =
    Complex(this.real * that, this.imag * that)

  def /(that : Complex) = {
    val denom = that.real * that.real + that.imag * that.imag
    Complex((this.real * that.real + this.imag * that.imag) / denom,
      (this.imag * that.real - this.real * that.imag) / denom)
  }

  def /(that : Int) =
    Complex(this.real / that, this.imag / that)

  def /(that : Long) =
    Complex(this.real / that, this.imag / that)

  def /(that : Float) =
    Complex(this.real / that, this.imag / that)

  def /(that : Double) =
    Complex(this.real / that, this.imag / that)

  def %(that: Complex) = {
    val div = this./(that)
    this - (Complex(floor(div.real), floor(div.imag)) * div)
  }

  def %(that : Int): Complex = this.%(Complex(that,0))
  def %(that : Long): Complex = %(Complex(that,0))
  def %(that : Float): Complex = %(Complex(that,0))
  def %(that : Double): Complex = %(Complex(that,0))

  def unary_- =
    Complex(-real, -imag)

  def abs =
    sqrt(real*real + imag*imag)

  def conjugate =
    Complex(real, -imag)

  def log =
    Complex(Math.log(abs), atan2(imag, real))

  def exp = {
    val expreal = Math.exp(real)
    Complex(expreal * cos(imag), expreal * sin(imag))
  }

  def pow(b: Double): Complex = pow(Complex(b, 0))

  def pow(b: Complex): Complex = {
    if (b == Complex.zero) Complex.one
    else if (this == Complex.zero) {
      if (b.imag != 0.0 || b.real < 0.0) Complex.nan
      else Complex.zero
    } else {
      val c = log * b
      val expReal = Math.exp(c.real)
      Complex(expReal * cos(c.imag), expReal * sin(c.imag))
    }
  }

  override def equals(that : Any) = that match {
    case that : Complex => this.real == that.real && this.imag == that.imag
    case real : Double => this.real == real && this.imag == 0
    case real : Int => this.real == real && this.imag == 0
    case real : Short => this.real == real && this.imag == 0
    case real : Long => this.real == real && this.imag == 0
    case real : Float => this.real == real && this.imag == 0
    case _ => false
  }

  // ensure hashcode contract is maintained for comparison to non-Complex numbers
  // x ^ 0 is x
  override val hashCode: Int = real.## ^ imag.##

  val norm: Double = real * real + imag * imag
}

object Complex{
  val zero =  Complex(0,0)

  /** Constant Complex(1,0). */
  val one =  Complex(1,0)

  /** Constant Complex(NaN, NaN). */
  val nan =  Complex(Double.NaN, Double.NaN)

  /** Constant Complex(0,1). */
  val i =  Complex(0,1)
//
//  implicit def toIComplexNumber(value: Complex): IComplexNumber =  Nd4j.createComplexNumber(value.real, value.imag)
//  implicit def fromIComplexNumber(value: IComplexNumber): Complex = Complex(value.realComponent.asInstanceOf[Double], value.imaginaryComponent.asInstanceOf[Double])
//
//  implicit val c2cbf = new CanBuildFrom[Seq[Complex], IComplexNumber, Seq[IComplexNumber]] {
//    import scala.collection.mutable.Builder
//
//    class A2Builder extends Builder[IComplexNumber, Seq[IComplexNumber]] {
//      var es = Seq.empty[IComplexNumber]
//      def +=(elem: IComplexNumber): this.type = {  es = es :+ elem; this }
//      def clear(): Unit = es = Nil
//      def result(): Seq[IComplexNumber] = es
//    }
//    def apply() = new A2Builder
//    def apply(from: Seq[Complex]) = apply()
//  }
}