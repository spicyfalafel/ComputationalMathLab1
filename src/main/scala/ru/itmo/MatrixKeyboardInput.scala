package ru.itmo

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object MatrixKeyboardInput {

  @tailrec
  def getRows: Int = try {
    val rows = readLine("Enter number of rows: ").toInt
    rows
  } catch {
    case _: NumberFormatException => getRows
  }

  def getMatrixFromInput: Array[Array[Double]] = {
    var matrix: Array[Array[Double]] = Array()
    val rows = getRows
    for (i <- 1 to rows by +1) {
      val line = readLine(s"Enter row $i(including b$i):")
      try {
        val nums: Array[Double] = line.split("\\s+").map(str => str.toDouble)
        if (nums.length != rows + 1) throw new IllegalArgumentException(s"Enter $rows x ${rows + 1} including b!")
        matrix = matrix :+ nums
      } catch {
        case _: Exception => getMatrixFromInput
      }
    }
    matrix
  }

  def setScale(number: Double, decimals: Int = 2): Double = {
    BigDecimal(number).setScale(decimals, BigDecimal.RoundingMode.HALF_UP).toDouble
  }

  def printMatrix(matrix: Array[Array[Double]], decimals: Int = 2): Unit = {
    println("===================")
    for (i <- matrix.indices) {
      for (j <- matrix(i).indices) {
        val el = setScale(matrix(i)(j), decimals)
        print(el + "  ")
      }
      println()
    }
  }
}
