package ru.itmo

import scala.io.StdIn.readLine

object MatrixKeyboardInput {

  def getMatrixFromInput: Array[Array[Double]] = {
    var matrix:Array[Array[Double]] = Array()
    val rows = readLine("Enter number of rows: ").toInt
    for (i <- 1 to rows by +1) {
      val line = readLine(s"Enter row $i(including b$i):")
      val nums: Array[Double] = line.split("\\s+").map(str => str.toDouble)
      matrix = matrix :+ nums
    }
    matrix
  }


  def printMatrix(matrix: Array[Array[Double]], decimals: Int = 2): Unit = {
    println("===================")
    for (i <- matrix.indices) {
      for (j <- matrix(i).indices) {
        val el = BigDecimal(matrix(i)(j)).setScale(decimals, BigDecimal.RoundingMode.HALF_UP).toDouble
        print(el + "  ")
      }
      println()
    }
  }
}
