package org.quantlib.math.distributions

import org.scalatest._
import org.quantlib.math.Comparing._

/**
  * Created by neo on 04/04/2017.
  */
class PoissonDistributionSuite extends FunSuite {

  test("Testing Poisson distributions...") {

    0.0 to(10.0, 0.5) foreach { mean =>

      val pdf = PoissonDistribution(mean)
      var logHelper = -mean
      val calculated = pdf(0)
      val expected = Math.exp(logHelper)
      val error = Math.abs(calculated - expected)
      assert(error <= 1.0e-16, s"Poisson pdf($mean)(0) calculated: $calculated expected: $expected error:  $error")

      1 until 25 foreach { i =>
        val calculated = pdf(i)
        val expected = if (mean == 0.0) 0.0 else {
          logHelper = logHelper + Math.log(mean) - Math.log(i.toDouble)
          Math.exp(logHelper)
        }
        val error = Math.abs(calculated - expected)

        assert(error <= 1.0e-13, s"Poisson pdf($mean)($i) calculated: $calculated expected: $expected error:  $error")
      }
    }

  }


  test("Testing cumulative Poisson distributions...") {
    0.0 to(10.0, 0.5) foreach { mean =>
      val cdf = CumulativePoissonDistribution(mean)
      var logHelper = -mean
      val cumCalculated = cdf(0)
      var cumExpected = Math.exp(logHelper)
      val error = Math.abs(cumCalculated - cumExpected)
      assert(error <= 1.0e-13, s"Poisson cdf($mean)(0) calculated: $cumCalculated expected: $cumExpected error:  $error")

      1 until 25 foreach { i =>
        val cumCalculated = cdf(i)
        cumExpected = if (mean == 0.0) 1.0 else {
          logHelper = logHelper + Math.log(mean) - Math.log(i.toDouble)
          cumExpected + Math.exp(logHelper)
        }
        val error = Math.abs(cumCalculated - cumExpected)

        assert(error <= 1.0e-12, s"Poisson cdf($mean)($i) calculated: $cumCalculated expected: $cumExpected error:  $error")
      }
    }

  }

  test("Testing inverse cumulative Poisson distributions...") {

    val icp = InverseCumulativePoisson()

    val data = Seq(
      0.2,
      0.5,
      0.9,
      0.98,
      0.99,
      0.999,
      0.9999,
      0.99995,
      0.99999,
      0.999999,
      0.9999999,
      0.99999999)

    data.zipWithIndex foreach { case (d, i) =>
      assert(icp(d) =~ i.toDouble,
        s"failed to reproduce known value for x = $d, calculated: ${icp(d)}  expected:  ${i.toDouble}")

    }

  }

}