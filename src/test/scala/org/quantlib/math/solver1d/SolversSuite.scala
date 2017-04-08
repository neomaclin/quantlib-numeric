package org.quantlib.math.solver1d

import org.scalatest._
import org.quantlib.math.solver1d.Solver1D._

/**
  * Created by neo on 05/04/2017.
  */
class SolversSuite extends FunSuite {

  final case class Probe(previous: Double, offset: Double) {
    private var applingOn: Double = previous

    def result:Double = applingOn

    def apply(x: Double): Double = {
      this.applingOn = x
      previous + offset - x * x
    }

    def derivative(x: Double) = 2.0 * x

  }

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

  def testNotBracketed(solver: SolverIDImPl, name: String, f: StatFunction, guess: Double): Unit = {
    val accuracies = Seq(1.0e-4, 1.0e-6, 1.0e-8)
    val expected = 1.0

    accuracies foreach { accuracy =>
      val root = solver.solve(f, accuracy, guess, 0.1)
      assert(Math.abs(root - expected) <= accuracy)

    }

  }

  def testBracketed(solver: SolverIDImPl, name: String, f: StatFunction, guess: Double): Unit = {
    val accuracies = Seq(1.0e-4, 1.0e-6, 1.0e-8)
    val expected = 1.0

    accuracies foreach { accuracy =>
      val root = solver.solve(f, accuracy, guess, 0.0, 2.0)
      assert(Math.abs(root - expected) <= accuracy)

    }

  }

  val mins = Seq(3.0, 2.25, 1.5, 1.0)
  val maxs = Seq(7.0, 5.75, 4.5, 3.0)
  val steps = Seq(0.2, 0.2, 0.1, 0.1)
  val offsets = Seq(25.0, 11.0, 5.0, 1.0)
  val guesses = Seq(4.5, 4.5, 2.5, 2.5)
  var argument = 0.0
  def testLastCallwithRoot(solver: SolverIDImPl, name: String, bracketed: Boolean, accuracy: Double): Unit = {
    (mins zip maxs zip steps zip offsets zip guesses) foreach {
      case (((((min), max), step), offset), guess) =>
        val probe = Probe(argument, offset)
        val result = if (bracketed) {
          solver.solve(probe, accuracy, guess, min, max)
        } else {
          solver.solve(probe, accuracy, guess, step)
        }
        argument = probe.result
        val error = Math.abs(result-argument)
        assert(result == argument)

    }
  }

  def testSolver(solver: SolverIDImPl, name: String, accuracy: Double): Unit = {
    testNotBracketed(solver, name, F1, 0.5)
    testBracketed(solver, name, F1, 0.5)
    testNotBracketed(solver, name, F1, 1.5)
    testBracketed(solver, name, F1, 1.5)
    //    // guess on the left side of the root, decreasing function
    testNotBracketed(solver, name, F2, 0.5)
    testBracketed(solver, name, F2, 0.5)
    //    // guess on the right side of the root, decreasing function
    testNotBracketed(solver, name, F2, 1.5)
    testBracketed(solver, name, F2, 1.5)
    testNotBracketed(solver, name, F3, 1.00001)
    //testLastCallwithRoot(solver, name, false, accuracy)
    //testLastCallwithRoot(solver, name, true, accuracy)
  }

  test("Testing Brent solver...") {
    testSolver(Brent(), "Brent", 1.0e-6)
  }

  test("Testing Bisection solver...") {
    testSolver(Bisection(), "Bisection", 1.0e-6)
  }

  test("Testing Newton solver...") {
    testSolver(Newton(), "Newton", 1.0e-12)
  }

  test("Testing NewtonSafe solver...") {
    testSolver(NewtonSafe(), "NewtonSafe", 1.0e-9)
  }

   test("Testing FalsePosition solver...") {
    testSolver(FalsePosition(), "FalsePosition", 1.0e-6)
  }

  test("Testing Ridder solver...") {
    testSolver(Ridder(), "Ridder", 1.0e-6)
  }

  test("Testing Secant solver...") {
    testSolver(Secant(), "Secant", 1.0e-6)
  }

  test("Testing FiniteDifferenceNewtonSafe solver...") {
    testSolver(FiniteDifferenceNewtonSafe(), "FiniteDifferenceNewtonSafe", 1.0e-6)
  }
}
