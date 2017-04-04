package org.quantlib.math

import org.quantlib.math._

package object copulas {


  @throws(classOf[IllegalArgumentException])
  def aliMikhailHaq(theta: Double, x: Double, y: Double): Double = AliMikhailHaqCopula(theta).apply(x, y)

  @throws(classOf[IllegalArgumentException])
  def clayton(theta: Double, x: Double, y: Double): Double = ClaytonCopula(theta).apply(x, y)

  @throws(classOf[IllegalArgumentException])
  def farlieGumbelMorgenstern(theta: Double, x: Double, y: Double): Double = FarlieGumbelMorgensternCopula(theta).apply(x, y)

  @throws(classOf[IllegalArgumentException])
  def frank(theta: Double, x: Double, y: Double): Double = FrankCopula(theta).apply(x, y)

  @throws(classOf[IllegalArgumentException])
  def galambos(theta: Double, x: Double, y: Double): Double = GalambosCopula(theta).apply(x, y)

//  @throws(classOf[IllegalArgumentException])
//  def gaussian(rho: Double, x: Double, y: Double): Double = GaussianCopula(rho).apply(x, y)

  @throws(classOf[IllegalArgumentException])
  def gumbel(theta: Double, x: Double, y: Double): Double = GumbelCopula(theta).apply(x, y)

  @throws(classOf[IllegalArgumentException])
  def huslerReiss(theta: Double, x: Double, y: Double): Double = HuslerReissCopula(theta).apply(x, y)

  @throws(classOf[IllegalArgumentException])
  def marshallOlkin(a1: Double, a2: Double, x: Double, y: Double): Double = MarshallOlkinCopula(a1, a2).apply(x, y)

  @throws(classOf[IllegalArgumentException])
  def plackett(theta: Double, x: Double, y: Double): Double = PlackettCopula(theta).apply(x, y)

  private[copulas] sealed trait Copula extends ((Double, Double) => Double) {

    protected def checkParam(x: Double, y: Double): Unit = {
      require(x >= 0.0 && x <= 1.0, s"1st argument ($x) must be in [0,1]")
      require(y >= 0.0 && y <= 1.0, s"2nd argument ($y) must be in [0,1]")
    }
  }

  @throws(classOf[IllegalArgumentException])
  val independent: Copula = new Copula {
    def apply(x: Double, y: Double): Double = {
      checkParam(x, y)
      x * y
    }
  }

  @throws(classOf[IllegalArgumentException])
  val max: Copula = new Copula {
    def apply(x: Double, y: Double): Double = {
      checkParam(x, y)
      Math.min(x, y)
    }
  }

  //@throws(classOf[IllegalArgumentException])	
  val min: Copula = new Copula {
    def apply(x: Double, y: Double): Double = {
      checkParam(x, y)
      Math.min(x + y - 1.0, 0.0)
    }
  }


  final case class AliMikhailHaqCopula(theta: Double) extends Copula {
    require(theta >= -1.0 && theta <= 1.0, s"theta ($theta) must be in [-1,1]")

    def apply(x: Double, y: Double): Double = {
      checkParam(x, y)

      (x * y) / (1.0 - theta * (1.0 - x) * (1.0 - y))
    }
  }


  final case class ClaytonCopula(theta: Double) extends Copula {
    require(theta >= -1.0, s"theta ($theta) must be greater or equal to -1")
    require(theta != 0.0, s"theta ($theta) must be different from 0")

    def apply(x: Double, y: Double): Double = {
      checkParam(x, y)

      Math.max(Math.pow(Math.pow(x, -theta) + Math.pow(y, -theta) - 1.0, -1.0 / theta), 0.0)
    }
  }

  final case class FarlieGumbelMorgensternCopula(theta: Double) extends Copula {
    require(theta >= -1.0 && theta <= 1.0, s"theta ($theta) must be in [-1,1]")

    def apply(x: Double, y: Double): Double = {
      checkParam(x, y)

      x * y + theta * x * y * (1.0 - x) * (1.0 - y)
    }
  }


  final case class FrankCopula(theta: Double) extends Copula {
    require(theta != 0.0, s"theta ($theta) must be different from 0")

    def apply(x: Double, y: Double): Double = {
      checkParam(x, y)
      -1.0 / theta * Math.log(1 + (Math.exp(-theta * x) - 1) * (Math.exp(-theta * y) - 1) / (Math.exp(-theta) - 1))
    }
  }


  final case class GalambosCopula(theta: Double) extends Copula {
    require(theta >= 0.0, s"theta ($theta) must be greater or equal to 0")

    def apply(x: Double, y: Double): Double = {
      checkParam(x, y)
      import Math._
      x * y * exp(pow(pow(-log(x), -theta) + pow(-log(y), -theta), -1 / theta));
    }
  }


  //  final case class GaussianCopula(rho: Double) extends Copula{
  //    require(rho >= -1.0 && rho <= 1.0 , s"rho ($rho) must be in [-1,1]")
  //
  //    def apply(x: Double, y: Double): Double = {
  //      checkParam(x,y)
  //      //bivariateNormalCdf(invCumNormal(x), invCumNormal(y))
  //    }
  //  }


  final case class GumbelCopula(theta: Double) extends Copula {
    require(theta >= 1.0, s"theta ($theta) must be greater or equal to 1")

    def apply(x: Double, y: Double): Double = {
      checkParam(x, y)
      import Math._
      exp(-pow(pow(-log(x), theta) + pow(-log(y), theta), 1 / theta))
    }
  }


  final case class HuslerReissCopula(theta: Double) extends Copula {
    require(theta >= 0.0, s"theta ($theta) must be greater or equal to 0")

    def apply(x: Double, y: Double): Double = {
      checkParam(x, y)
      import Math._
      import distributions._
      pow(x, cumNormal(1.0 / theta + 0.5 * theta * log(-log(x) / -log(y)))) *
        pow(y, cumNormal(1.0 / theta + 0.5 * theta * log(-log(y) / -log(x))))
    }
  }


  final case class MarshallOlkinCopula(a1: Double, a2: Double) extends Copula {
    private val a1_local = 1.0 - a1
    private val a2_local = 1.0 - a2
    require(a1_local >= 0.0, s"1st parameter ($a1_local) must be non-negative")
    require(a2_local >= 0.0, s"2nd parameter ($a2_local) must be non-negative")

    def apply(x: Double, y: Double): Double = {
      checkParam(x, y)
      Math.min(y * Math.pow(x, a1_local), x * Math.pow(y, a2_local))
    }
  }

  final case class PlackettCopula(theta: Double) extends Copula {
    require(theta >= 0.0, s"theta ($theta) must be greater or equal to 0")
    require(theta != 1.0, s"theta ($theta) must be different from 1")

    def apply(x: Double, y: Double): Double = {
      checkParam(x, y)
      import Math._
      ((1.0 + (theta - 1.0) * (x + y)) -
        sqrt(pow(1.0 + (theta - 1.0) * (x + y), 2.0) -
          4.0 * x * y * theta * (theta - 1.0))) / (2.0 * (theta - 1.0))
    }
  }

}
