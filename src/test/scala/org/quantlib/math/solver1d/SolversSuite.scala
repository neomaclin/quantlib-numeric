package org.quantlib.math.solver1d

import org.scalatest._
import org.quantlib.math.solver1d.Solver1D._

/**
  * Created by neo on 05/04/2017.
  */
class SolversSuite extends FunSuite {

  object F1 {

    def apply(x: Double) = x * x - 1.0

    def derivative(x: Double) = 2.0 * x
  }

  object F2 {

    def apply(x: Double) = 1.0 - x * x

    def derivative(x: Double) = -2.0 * x
  }

  object F3 {

    def apply(x: Double) = Math.atan(x - 1)

    def derivative(x: Double) = 1.0 / (1.0 + (x - 1.0) * (x - 1.0))
  }

  def testNotBracketed(solver: SolverIDImPl, name: String, f: Double => Double, guess: Double) {
    val accuracies = Seq(1.0e-4, 1.0e-6, 1.0e-8)
    val expected = 1.0

    accuracies foreach { accuracy =>
      val root = solver.solve(f, accuracy, guess, 0.1)
      assert(Math.abs(root - expected) <= accuracy)
      //{
      //        s"$solver solver (not bracketed):\n"
      //          << "    expected:   " << expected << "\n"
      //          << "    calculated: " << root << "\n"
      //          << "    accuracy:   " << accuracy[i]);
      //      }
    }

  }

  def testBracketed(solver: SolverIDImPl, name: String, f: Double => Double, guess: Double) {
    val accuracies = Seq(1.0e-4, 1.0e-6, 1.0e-8)
    val expected = 1.0

    accuracies foreach { accuracy =>
      val root = solver.solve(f, accuracy, guess,0.0, 2.0)
      assert(Math.abs(root - expected) <= accuracy)
      //{
      //        s"$solver solver (not bracketed):\n"
      //          << "    expected:   " << expected << "\n"
      //          << "    calculated: " << root << "\n"
      //          << "    accuracy:   " << accuracy[i]);
      //      }
    }

  }

  test("Testing Brent solver...") {
   // testNotBracketed(Brent(), "Brent", F1.apply, 0.5)
    testBracketed(Brent(), "Brent", F1.apply, 0.5)
   // testNotBracketed(solver, name, F1(), 1.5);
    testBracketed(Brent(), "Brent", F1.apply, 1.5)
    // guess on the left side of the root, decreasing function
 //   testNotBracketed("Brent", name, F2(), 0.5);
    testBracketed(Brent(), "Brent", F2.apply, 0.5)
    // guess on the right side of the root, decreasing function
  //  testNotBracketed(solver, name, F2(), 1.5);
    testBracketed(Brent(), "Brent", F2.apply, 1.5)
   // testNotBracketed(solver, name, F3.apply, 1.00001);
  }
}
