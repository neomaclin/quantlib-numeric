package org.quantlib.math

object Calculation {

  trait NumberLike[T]

  type Operator[A] = (A, A) => A

  trait Arithmetic[A <: NumberLike[A]]{
    def +(x: A, y: A): A
    def -(x: A, y: A): A
    def *(x: A, y: A): A
    def /(x: A, y: A): A
  }

  trait ArithmeticOps[A <: NumberLike[A]] {
    def +(y: A): A
    def -(y: A): A
    def *(y: A): A
    def /(y: A): A
  }

}
