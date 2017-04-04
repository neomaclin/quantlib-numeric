package org.quantlib.math

import org.quantlib.math
import Rounding._
import Comparing._
import org.scalatest._


/**
  * Created by neo on 03/04/2017.
  */
class RoundingSuite extends FunSuite {

  final case class TestCase(x: Double,
                            precision: Int,
                            closest: Double,
                            up: Double,
                            down: Double,
                            floor: Double,
                            ceiling: Double)

  val testData = Seq(
    TestCase(0.86313513,  5, 0.86314,   0.86314,  0.86313,  0.86314,  0.86313),
    TestCase(0.86313,     5, 0.86313,   0.86313,  0.86313,  0.86313,  0.86313),
    TestCase(-7.64555346, 1, -7.6,      -7.7,     -7.6,     -7.6,    -7.6),
    TestCase(0.13961605,  2, 0.14,      0.14,     0.13,     0.14,     0.13),
    TestCase(0.14344179,  4, 0.1434,    0.1435,   0.1434,   0.1434,   0.1434),
    TestCase(-4.74315016, 2, -4.74,     -4.75,    -4.74,    -4.74,    -4.74),
    TestCase(-7.82772074, 5, -7.82772,  -7.82773, -7.82772, -7.82772, -7.82772),
    TestCase(2.74137947,  3, 2.741,     2.742,    2.741,    2.741,    2.741),
    TestCase(2.13056714,  1, 2.1,       2.2,      2.1,      2.1,      2.1),
    TestCase(-1.06228670, 1, -1.1,      -1.1,     -1.0,     -1.0,     -1.1),
    TestCase(8.29234094,  4, 8.2923,    8.2924,   8.2923,   8.2923,   8.2923),
    TestCase(7.90185598,  2, 7.90,      7.91,     7.90,     7.90,     7.90),
    TestCase(-0.26738058, 1, -0.3,      -0.3,     -0.2,     -0.2,     -0.3),
    TestCase(1.78128713,  1, 1.8,       1.8,      1.7,      1.8,      1.7),
    TestCase(4.23537260,  1, 4.2,       4.3,      4.2,      4.2,      4.2),
    TestCase(3.64369953,  4, 3.6437,    3.6437,   3.6436,   3.6437,    3.6436),
    TestCase(6.34542470,  2, 6.35,      6.35,     6.34,     6.35,     6.34),
    TestCase(-0.84754962, 4, -0.8475,   -0.8476,  -0.8475,  -0.8475,  -0.8475),
    TestCase(4.60998652,  1, 4.6,       4.7,      4.6,      4.6,      4.6),
    TestCase(6.28794223,  3, 6.288,     6.288,    6.287,    6.288,    6.287),
    TestCase(7.89428221,  2, 7.89,      7.90,     7.89,     7.89,     7.89)
  )

  test("Testing closest decimal rounding...") {
    testData foreach { case TestCase(original, precision, expected, _, _, _, _) =>

      val calculated = ClosestRounding(precision = precision).apply(original)

      assert(calculated == expected,
        s"Original number: $original, Expected: $expected, Calculated: $calculated")
    }

  }

  test("Testing upward decimal rounding...") {
    testData foreach { case TestCase(original, precision, _, expected, _, _, _) =>

      val calculated = UpRounding(precision = precision).apply(original)

      assert(calculated == expected,
        s"Original number: $original, Expected: $expected, Calculated: $calculated")
    }

  }

  test("Testing downward decimal rounding...") {

    testData foreach { case TestCase(original, precision, _, _, expected, _, _) =>

      val calculated = DownRounding(precision = precision).apply(original)

      assert(calculated == expected,
        s"Original number: $original, Expected: $expected, Calculated: $calculated")
    }

  }

  test("Testing floor decimal rounding...") {
    testData foreach { case TestCase(original, precision, _, _, _, expected, _) =>

      val calculated = FloorRounding(precision = precision).apply(original)

      assert(calculated == expected,
        s"Original number: $original, Expected: $expected, Calculated: $calculated")
    }

  }

  test("Testing ceiling decimal rounding...") {

    testData foreach { case TestCase(original, precision, _, _, _, _, expected) =>

      val calculated = CeilingRounding(precision = precision).apply(original)

      assert(calculated == expected,
        s"Original number: $original, Expected: $expected, Calculated: $calculated")
    }

  }
}
