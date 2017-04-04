package org.quantlib.math.solver1d

import org.quantlib.math.functions.StatisticFunction

/**
  * Created by Neo on 2016/7/30.
//  */
//case class Bisection( maxEvaluations: Int,
//                 lowerBound: Double,
//                 upperBound: Double,
//                 lowerBoundEnforced: Boolean = false,
//                 upperBoundEnforced: Boolean = false)
//  extends Solver1D
//{
//  def solve(f: StatisticFunction, xAccuracy: Double): Double = {
//
//    Real dx, xMid, fMid;
//
//    // Orient the search so that f>0 lies at root_+dx
//    if (fxMin_ < 0.0) {
//      dx = xMax_-xMin_;
//      root_ = xMin_;
//    } else {
//      dx = xMin_-xMax_;
//      root_ = xMax_;
//    }
//
//    while (evaluationNumber_<=maxEvaluations_) {
//      dx /= 2.0;
//      xMid = root_+dx;
//      fMid = f(xMid);
//      ++evaluationNumber_;
//      if (fMid <= 0.0)
//        root_ = xMid;
//      if (std::fabs(dx) < xAccuracy || (close(fMid, 0.0))) {
//        f(root_);
//        ++evaluationNumber_;
//        return root_;
//      }
//    }
//    assert("maximum number of function evaluations ("
//      << maxEvaluations_ << ") exceeded");
//  }
//  }
//}
