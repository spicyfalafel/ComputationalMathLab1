package ru.itmo

object Determinant {


  def calculate(matrix: List[List[Double]], n: Int): Double = {
    if (n == 1) matrix(0)(0)
    if (n == 2) matrix(0)(0) * matrix(1)(1) + matrix(0)(1) * matrix(1)(0)
    0 //todo
  }

}
