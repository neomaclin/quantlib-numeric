package org.quantlib.math.distributions

import org.scalatest._
import org.quantlib.math.Constants._
import org.quantlib.math.Utilities
import org.quantlib.math.distributions.bivariate._
import org.scalactic.Tolerance



/**
  * Created by neo on 05/03/2017.
  */
class BivariateDistributionsSuite extends FunSuite {

  final case class BivariateTestData(a: Double, b:Double, rho: Double, result: Double)
  
  val bcdTestSet = Seq(
    
    BivariateTestData( 0.0,  0.0,  0.0, 0.250000 ),
    BivariateTestData( 0.0,  0.0, -0.5, 0.166667 ),
    BivariateTestData( 0.0,  0.0,  0.5, 1.0/3    ),
    BivariateTestData( 0.0, -0.5,  0.0, 0.154269 ),
    BivariateTestData( 0.0, -0.5, -0.5, 0.081660 ),
    BivariateTestData( 0.0, -0.5,  0.5, 0.226878 ),
    BivariateTestData( 0.0,  0.5,  0.0, 0.345731 ),
    BivariateTestData( 0.0,  0.5, -0.5, 0.273122 ),
    BivariateTestData( 0.0,  0.5,  0.5, 0.418340 ),

    BivariateTestData(-0.5,  0.0,  0.0, 0.154269 ),
    BivariateTestData(-0.5,  0.0, -0.5, 0.081660 ),
    BivariateTestData(-0.5,  0.0,  0.5, 0.226878 ),
    BivariateTestData(-0.5, -0.5,  0.0, 0.095195 ),
    BivariateTestData(-0.5, -0.5, -0.5, 0.036298 ),
    BivariateTestData(-0.5, -0.5,  0.5, 0.163319 ),
    BivariateTestData(-0.5,  0.5,  0.0, 0.213342 ),
    BivariateTestData(-0.5,  0.5, -0.5, 0.145218 ),
    BivariateTestData(-0.5,  0.5,  0.5, 0.272239 ),

    BivariateTestData( 0.5,  0.0,  0.0, 0.345731 ),
    BivariateTestData( 0.5,  0.0, -0.5, 0.273122 ),
    BivariateTestData( 0.5,  0.0,  0.5, 0.418340 ),
    BivariateTestData( 0.5, -0.5,  0.0, 0.213342 ),
    BivariateTestData( 0.5, -0.5, -0.5, 0.145218 ),
    BivariateTestData( 0.5, -0.5,  0.5, 0.272239 ),
    BivariateTestData( 0.5,  0.5,  0.0, 0.478120 ),
    BivariateTestData( 0.5,  0.5, -0.5, 0.419223 ),
    BivariateTestData( 0.5,  0.5,  0.5, 0.546244 ),

    
    BivariateTestData( 0.0, 0.0, Math.sqrt(1/2.0), 3.0/8),

   
    BivariateTestData( 0.0,   30, -1.0, 0.500000 ),
    BivariateTestData( 0.0,   30,  0.0, 0.500000 ),
    BivariateTestData( 0.0,   30,  1.0, 0.500000 ),

  
    BivariateTestData( 30,   30,  -1.0, 1.000000 ),
    BivariateTestData( 30,   30,   0.0, 1.000000 ),
    BivariateTestData( 30,   30,   1.0, 1.000000 ),


    BivariateTestData(-30, -1.0,  -1.0, 0.000000 ),
    BivariateTestData(-30,  0.0,  -1.0, 0.000000 ),
    BivariateTestData(-30,  1.0,  -1.0, 0.000000 ),
    BivariateTestData(-30, -1.0,   0.0, 0.000000 ),
    BivariateTestData(-30,  0.0,   0.0, 0.000000 ),
    BivariateTestData(-30,  1.0,   0.0, 0.000000 ),
    BivariateTestData(-30, -1.0,   1.0, 0.000000 ),
    BivariateTestData(-30,  0.0,   1.0, 0.000000 ),
    BivariateTestData(-30,  1.0,   1.0, 0.000000 )
  )
  
  val rhos = Seq(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.99999)
  
  def checkBivariate(bivariate: (Double) => BivariateDistribution, tolerance: Double ): Unit ={
    bcdTestSet.foreach{ case BivariateTestData(a,b,rho,result) =>
      val bcd = bivariate(rho)
      val value = bcd.apply(a, b)

      assert (Math.abs(value-result) < tolerance,
        s"case failing at a: $a b: $b rho: $rho for reuslt $result with calculated: $value")

    }
  }

  def checkBivariateAtZero(bivariate: (Double) => BivariateDistribution, tolerance: Double ): Unit ={

    val x = 0.0
    val y = 0.0

    rhos.foreach { rho =>
     List(-1,1) foreach { sign =>
        val bvn =  bivariate(sign * rho)
        val expected = 0.25 + Math.asin(sign*rho) / (2*M_PI)
        val realised = bvn.apply(x,y)
        assert(Math.abs(realised-expected) < tolerance,
         s"case failing at rho: $rho for expected $expected with realised: $realised of tolerance: $tolerance")

     }
    }

  }

  def checkBivariateTail(bivariate: (Double) => BivariateDistribution, tolerance: Double ): Unit ={

    val x = -6.9
    val corr = -0.999
    val bvn = bivariate(corr)

    var y = 6.9
    0 until 10 foreach { i =>
      val cdf0 = bvn.apply(x,y)
      y = y + tolerance
      val cdf1 = bvn.apply(x,y)
      assert(cdf0 <= cdf1,
        s" cdf must be decreasing in the tail of cdf0: $cdf0 cdf1: $cdf1 x: $x  y: $y rho: $corr")
      }


  }

  test("Testing bivariate cumulative normal distribution..."){

    checkBivariate(BivariateCumulativeNormalDistributionDr78(_), 1.0e-6)
    checkBivariate(BivariateCumulativeNormalDistributionWe04DP(_), 1.0e-6)

    checkBivariateAtZero(BivariateCumulativeNormalDistributionDr78(_), 1.0e-5)
    checkBivariateAtZero(BivariateCumulativeNormalDistributionWe04DP(_), 1.0e-15)

    checkBivariateTail(BivariateCumulativeNormalDistributionDr78(_), 1.0e-5)
    checkBivariateTail(BivariateCumulativeNormalDistributionWe04DP(_), 1.0e-6)
    checkBivariateTail(BivariateCumulativeNormalDistributionWe04DP(_), 1.0e-8)
  }


  val xs = Seq( 0.00,  0.50,  1.00,  1.50,  2.00,  2.50, 3.00, 4.00,  5.00,  6.00,  7.00,  8.00, 9.00, 10.00 )
  val ns = Seq( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20, 25, 30, 60, 90, 120, 150, 300, 600 )
  // Part of table 1 from the reference paper
  val expected1 = Seq(
    0.33333,  0.50000,  0.63497,  0.72338,  0.78063,  0.81943,  0.84704,  0.88332,  0.90590,  0.92124,  0.93231,  0.94066,  0.94719,  0.95243,
    0.33333,  0.52017,  0.68114,  0.78925,  0.85607,  0.89754,  0.92417,  0.95433,  0.96978,  0.97862,  0.98411,  0.98774,  0.99026,  0.99208,
    0.33333,  0.52818,  0.70018,  0.81702,  0.88720,  0.92812,  0.95238,  0.97667,  0.98712,  0.99222,  0.99497,  0.99657,  0.99756,  0.99821,
    0.33333,  0.53245,  0.71052,  0.83231,  0.90402,  0.94394,  0.96612,  0.98616,  0.99353,  0.99664,  0.99810,  0.99885,  0.99927,  0.99951,
    0.33333,  0.53510,  0.71701,  0.84196,  0.91449,  0.95344,  0.97397,  0.99095,  0.99637,  0.99836,  0.99918,  0.99956,  0.99975,  0.99985,
    0.33333,  0.53689,  0.72146,  0.84862,  0.92163,  0.95972,  0.97893,  0.99365,  0.99779,  0.99913,  0.99962,  0.99982,  0.99990,  0.99995,
    0.33333,  0.53819,  0.72470,  0.85348,  0.92679,  0.96415,  0.98230,  0.99531,  0.99857,  0.99950,  0.99981,  0.99992,  0.99996,  0.99998,
    0.33333,  0.53917,  0.72716,  0.85719,  0.93070,  0.96743,  0.98470,  0.99639,  0.99903,  0.99970,  0.99990,  0.99996,  0.99998,  0.99999,
    0.33333,  0.53994,  0.72909,  0.86011,  0.93375,  0.96995,  0.98650,  0.99713,  0.99931,  0.99981,  0.99994,  0.99998,  0.99999,  1.00000,
    0.33333,  0.54056,  0.73065,  0.86247,  0.93621,  0.97194,  0.98788,  0.99766,  0.99950,  0.99988,  0.99996,  0.99999,  1.00000,  1.00000,
    0.33333,  0.54243,  0.73540,  0.86968,  0.94362,  0.97774,  0.99168,  0.99890,  0.99985,  0.99998,  1.00000,  1.00000,  1.00000,  1.00000,
    0.33333,  0.54338,  0.73781,  0.87336,  0.94735,  0.98053,  0.99337,  0.99932,  0.99993,  0.99999,  1.00000,  1.00000,  1.00000,  1.00000,
    0.33333,  0.54395,  0.73927,  0.87560,  0.94959,  0.98216,  0.99430,  0.99952,  0.99996,  1.00000,  1.00000,  1.00000,  1.00000,  1.00000,
    0.33333,  0.54433,  0.74025,  0.87709,  0.95108,  0.98322,  0.99489,  0.99963,  0.99998,  1.00000,  1.00000,  1.00000,  1.00000,  1.00000,
    0.33333,  0.54528,  0.74271,  0.88087,  0.95482,  0.98580,  0.99623,  0.99983,  0.99999,  1.00000,  1.00000,  1.00000,  1.00000,  1.00000,
    0.33333,  0.54560,  0.74354,  0.88215,  0.95607,  0.98663,  0.99664,  0.99987,  1.00000,  1.00000,  1.00000,  1.00000,  1.00000,  1.00000,
    0.33333,  0.54576,  0.74396,  0.88279,  0.95669,  0.98704,  0.99683,  0.99989,  1.00000,  1.00000,  1.00000,  1.00000,  1.00000,  1.00000,
    0.33333,  0.54586,  0.74420,  0.88317,  0.95706,  0.98729,  0.99695,  0.99990,  1.00000,  1.00000,  1.00000,  1.00000,  1.00000,  1.00000,
    0.33333,  0.54605,  0.74470,  0.88394,  0.95781,  0.98777,  0.99717,  0.99992,  1.00000,  1.00000,  1.00000,  1.00000,  1.00000,  1.00000,
    0.33333,  0.54615,  0.74495,  0.88432,  0.95818,  0.98801,  0.99728,  0.99993,  1.00000,  1.00000,  1.00000,  1.00000,  1.00000,  1.00000
  )
  // Part of table 2 from the reference paper
  val expected2 = Seq(
    0.16667,  0.36554,  0.54022,  0.65333,  0.72582,  0.77465,  0.80928,  0.85466,  0.88284,  0.90196,  0.91575,  0.92616,  0.93429,  0.94081,
    0.16667,  0.38889,  0.59968,  0.73892,  0.82320,  0.87479,  0.90763,  0.94458,  0.96339,  0.97412,  0.98078,  0.98518,  0.98823,  0.99044,
    0.16667,  0.39817,  0.62478,  0.77566,  0.86365,  0.91391,  0.94330,  0.97241,  0.98483,  0.99086,  0.99410,  0.99598,  0.99714,  0.99790,
    0.16667,  0.40313,  0.63863,  0.79605,  0.88547,  0.93396,  0.96043,  0.98400,  0.99256,  0.99614,  0.99782,  0.99868,  0.99916,  0.99944,
    0.16667,  0.40620,  0.64740,  0.80900,  0.89902,  0.94588,  0.97007,  0.98972,  0.99591,  0.99816,  0.99909,  0.99951,  0.99972,  0.99983,
    0.16667,  0.40829,  0.65345,  0.81794,  0.90820,  0.95368,  0.97607,  0.99290,  0.99755,  0.99904,  0.99958,  0.99980,  0.99989,  0.99994,
    0.16667,  0.40980,  0.65788,  0.82449,  0.91482,  0.95914,  0.98010,  0.99482,  0.99844,  0.99946,  0.99979,  0.99991,  0.99996,  0.99998,
    0.16667,  0.41095,  0.66126,  0.82948,  0.91981,  0.96314,  0.98295,  0.99605,  0.99895,  0.99968,  0.99989,  0.99996,  0.99998,  0.99999,
    0.16667,  0.41185,  0.66393,  0.83342,  0.92369,  0.96619,  0.98506,  0.99689,  0.99926,  0.99980,  0.99994,  0.99998,  0.99999,  1.00000,
    0.16667,  0.41257,  0.66608,  0.83661,  0.92681,  0.96859,  0.98667,  0.99748,  0.99946,  0.99987,  0.99996,  0.99999,  1.00000,  1.00000,
    0.16667,  0.41476,  0.67268,  0.84633,  0.93614,  0.97550,  0.99103,  0.99884,  0.99984,  0.99998,  1.00000,  1.00000,  1.00000,  1.00000,
    0.16667,  0.41586,  0.67605,  0.85129,  0.94078,  0.97877,  0.99292,  0.99930,  0.99993,  0.99999,  1.00000,  1.00000,  1.00000,  1.00000,
    0.16667,  0.41653,  0.67810,  0.85430,  0.94356,  0.98066,  0.99396,  0.99950,  0.99996,  1.00000,  1.00000,  1.00000,  1.00000,  1.00000,
    0.16667,  0.41698,  0.67947,  0.85632,  0.94540,  0.98189,  0.99461,  0.99962,  0.99998,  1.00000,  1.00000,  1.00000,  1.00000,  1.00000,
    0.16667,  0.41810,  0.68294,  0.86141,  0.94998,  0.98483,  0.99607,  0.99982,  0.99999,  1.00000,  1.00000,  1.00000,  1.00000,  1.00000,
    0.16667,  0.41847,  0.68411,  0.86312,  0.95149,  0.98577,  0.99651,  0.99987,  1.00000,  1.00000,  1.00000,  1.00000,  1.00000,  1.00000,
    0.16667,  0.41866,  0.68470,  0.86398,  0.95225,  0.98623,  0.99672,  0.99989,  1.00000,  1.00000,  1.00000,  1.00000,  1.00000,  1.00000,
    0.16667,  0.41877,  0.68505,  0.86449,  0.95270,  0.98650,  0.99684,  0.99990,  1.00000,  1.00000,  1.00000,  1.00000,  1.00000,  1.00000,
    0.16667,  0.41900,  0.68576,  0.86552,  0.95360,  0.98705,  0.99707,  0.99992,  1.00000,  1.00000,  1.00000,  1.00000,  1.00000,  1.00000,
    0.16667,  0.41911,  0.68612,  0.86604,  0.95405,  0.98731,  0.99719,  0.99993,  1.00000,  1.00000,  1.00000,  1.00000,  1.00000,  1.00000
  )

  test("Testing bivariate cumulative Student t distribution...") {
    val tolerance = 1.0e-5
    ns.zipWithIndex foreach { case(n,i) =>

      val f1 = BivariateCumulativeStudentDistribution(n,  0.5)
      val f2 = BivariateCumulativeStudentDistribution(n, -0.5)

      xs.zipWithIndex foreach{ case(x,j)=>

        val calculated1 = f1.apply(x, x)
        val reference1 = expected1(i*xs.length+j)
        val calculated2 = f2.apply(x, x)
        val reference2 = expected2(i*xs.length+j)

        assert(Math.abs(calculated1 - reference1)<=tolerance,
          s"Failed to reproduce CDF value at $x with calculated: $calculated1 and expected: $reference1" )
        assert(Math.abs(calculated2 - reference2)<=tolerance,
          s"Failed to reproduce CDF value at  $x with calculated: $calculated2 and expected: $reference2" )
      }
    }
  }

  final case class BivariateStudentTestData(n: Int, rho: Double, x: Double, y:Double, result: Double)

  val studentTestSet = Seq(
        BivariateStudentTestData(2,    -1.0,   5.0,   8.0,   0.973491),
        BivariateStudentTestData(2,     1.0,  -2.0,   8.0,   0.091752),
        BivariateStudentTestData(2,     1.0,   5.25, -9.5,   0.005450),
        BivariateStudentTestData(3,    -0.5,  -5.0,  -5.0,   0.000220),
        BivariateStudentTestData(4,    -1.0,  -8.0,   7.5,   0.0),
        BivariateStudentTestData(4,     0.5,  -5.5,  10.0,   0.002655),
        BivariateStudentTestData(4,     1.0,  -5.0,   6.0,   0.003745),
        BivariateStudentTestData(4,     1.0,   6.0,   5.5,   0.997336),
        BivariateStudentTestData(5,    -0.5,  -7.0,  -6.25,  0.000004),
        BivariateStudentTestData(5,    -0.5,   3.75, -7.25,  0.000166),
        BivariateStudentTestData(5,    -0.5,   7.75, -1.25,  0.133073),
        BivariateStudentTestData(6,     0.0,   7.5,   3.25,  0.991149),
        BivariateStudentTestData(7,    -0.5,  -1.0,  -8.5,   0.000001),
        BivariateStudentTestData(7,    -1.0,  -4.25, -4.0,   0.0),
        BivariateStudentTestData(7,     0.0,   0.5,  -2.25,  0.018819),
        BivariateStudentTestData(8,    -1.0,   8.25,  1.75,  0.940866),
        BivariateStudentTestData(8,     0.0,   2.25,  4.75,  0.972105),
        BivariateStudentTestData(9,    -0.5,  -4.0,   8.25,  0.001550),
        BivariateStudentTestData(9,    -1.0,  -1.25, -8.75,  0.0),
        BivariateStudentTestData(9,    -1.0,   5.75, -6.0,   0.0),
        BivariateStudentTestData(9,     0.5,  -6.5,  -9.5,   0.000001),
        BivariateStudentTestData(9,     1.0,  -2.0,   9.25,  0.038276),
        BivariateStudentTestData(10,   -1.0,  -0.5,   6.0,   0.313881),
        BivariateStudentTestData(10,    0.5,   0.0,   9.25,  0.5),
        BivariateStudentTestData(10,    0.5,   6.75, -2.25,  0.024090),
        BivariateStudentTestData(10,    1.0,  -1.75, -1.0,   0.055341),
        BivariateStudentTestData(15,    0.0,  -1.25, -4.75,  0.000029),
        BivariateStudentTestData(15,    0.0,  -2.0,  -1.5,   0.003411),
        BivariateStudentTestData(15,    0.5,   3.0,  -3.25,  0.002691),
        BivariateStudentTestData(20,   -0.5,   2.0,  -1.25,  0.098333),
        BivariateStudentTestData(20,   -1.0,   3.0,   8.0,   0.996462),
        BivariateStudentTestData(20,    0.0,  -7.5,   1.5,   0.0),
        BivariateStudentTestData(20,    0.5,   1.25,  9.75,  0.887136),
        BivariateStudentTestData(25,   -1.0,  -4.25,  5.0,   0.000111),
        BivariateStudentTestData(25,    0.5,   9.5,  -1.5,   0.073069),
        BivariateStudentTestData(25,    1.0,  -6.5,  -3.25,  0.0),
        BivariateStudentTestData(30,   -1.0,  -7.75, 10.0,   0.0),
        BivariateStudentTestData(30,    1.0,   0.5,   9.5,   0.689638),
        BivariateStudentTestData(60,   -1.0,  -3.5,  -8.25,  0.0),
        BivariateStudentTestData(60,   -1.0,   4.25,  0.75,  0.771869),
        BivariateStudentTestData(60,   -1.0,   5.75,  3.75,  0.9998),
        BivariateStudentTestData(60,    0.5,  -4.5,   8.25,  0.000016),
        BivariateStudentTestData(60,    1.0,   6.5,  -4.0,   0.000088),
        BivariateStudentTestData(90,   -0.5,  -3.75, -2.75,  0.0),
        BivariateStudentTestData(90,    0.5,   8.75, -7.0,   0.0),
        BivariateStudentTestData(120,   0.0,  -3.5,  -9.25,  0.0),
        BivariateStudentTestData(120,   0.0,  -8.25,  5.0,   0.0),
        BivariateStudentTestData(120,   1.0,  -0.75,  3.75,  0.227361),
        BivariateStudentTestData(120,   1.0,  -3.5,  -8.0,   0.0),
        BivariateStudentTestData(150,   0.0,  10.0,  -1.75,  0.041082),
        BivariateStudentTestData(300,  -0.5,  -6.0,   3.75,  0.0),
        BivariateStudentTestData(300,  -0.5,   3.5,  -4.5,   0.000004),
        BivariateStudentTestData(300,   0.0,   6.5,  -5.0,   0.0),
        BivariateStudentTestData(600,  -0.5,   9.25,  1.5,   0.93293),
        BivariateStudentTestData(600,  -1.0,  -9.25,  1.5,   0.0),
        BivariateStudentTestData(600,   0.5,  -5.0,   8.0,   0.0),
        BivariateStudentTestData(600,   1.0,  -2.75, -9.0,   0.0),
        BivariateStudentTestData(1000, -0.5,  -2.5,   0.25,  0.000589),
        BivariateStudentTestData(1000, -0.5,   3.0,   1.0,   0.839842),
        BivariateStudentTestData(2000, -1.0,   9.0,  -4.75,  0.000001),
        BivariateStudentTestData(2000,  0.5,   9.75,  7.25,  1.0),
        BivariateStudentTestData(2000,  1.0,   0.75, -9.0,   0.0),
        BivariateStudentTestData(5000, -0.5,   9.75,  5.5,   1.0),
        BivariateStudentTestData(5000, -1.0,   6.0,   1.0,   0.841321),
        BivariateStudentTestData(5000,  1.0,   4.0,  -7.75,  0.0),
        BivariateStudentTestData(10000, 0.5,   1.5,   6.0,   0.933177)
  )

  test("Testing bivariate cumulative Student t distribution with some random data...") {
    val tolerance = 1.0e-6
    studentTestSet foreach { case BivariateStudentTestData(n, rho, x, y, expected) =>

      val f = BivariateCumulativeStudentDistribution(n, rho)
      val calculated = f(x, y)

      assert(Math.abs(calculated - expected) <= tolerance,
        s"Failed to reproduce CDF value: n: $n rho: $rho x: $x y: $y calculated: $calculated expected: $expected")
    }
  }

  test("Testing bivariate cumulative Student t distribution for large N...") {
    -1.0 until (1.01,0.25) foreach{ rho =>
      val T = BivariateCumulativeStudentDistribution(10000, rho)
      val N = BivariateCumulativeNormalDistributionWe04DP(rho)

      val tolerance = 4.0e-5
      var avgDiff = 0.0
      var m = 0

      -10.0 until (10.1, 0.25) foreach { x=>
        -10.0 until (10.1, 0.25) foreach { y=>
        val calculated = T.apply(x, y)
        val expected = N.apply(x, y)
        val diff = Math.abs(calculated - expected)
        assert( diff <= tolerance,
          s"Failed to reproduce limit value:  rho: $rho x: $x  y: $y calculated: $calculated expected:  $expected")

          avgDiff += diff;
          m = m + 1
        }
      }

      avgDiff = avgDiff/ m
      assert(avgDiff <= 3.0e-6, s"Failed to reproduce average limit value:  rho: $rho average error: $avgDiff")

    }
  }
}
