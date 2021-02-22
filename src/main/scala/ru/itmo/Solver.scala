package ru.itmo

import ru.itmo.MatrixKeyboardInput.printMatrix

import scala.math.{abs, pow}

object Solver {

  def solve(triangleMatrix: Array[Array[Double]]): Array[Double] = {
    var res: Map[Int, Double] = Map()
    val lastRowIndex = triangleMatrix.length - 1

    for (i <- lastRowIndex to 0 by -1) {
      val xi = getXFromRow(triangleMatrix(i), res)
      res = res + (i + 1 -> xi)
    }

    res.values.toArray.reverse
  }

  def getXFromRow(line: Array[Double], xMap: Map[Int, Double]): Double = {
    val t = line.dropWhile(num => num == 0)
    val first = line.indexWhere(num => num != 0)
    var x: Double = line.last
    if (t.length == 2) {
      return x / t(0)
    }
    for (i <- line.length - 2 to first + 1 by -1) {
      val xi: Double = xMap(i + 1)
      x -= xi * line(i)
    }
    x / t(0)
  }

  def checkAnswers(matrix: Array[Array[Double]], answers: Array[Double]) = {
    var res: Array[Double] = Array()
    for (i <- matrix.indices) {
      var left:Double = 0
      for (j <- 0 to matrix(i).length - 2 by +1) {
        left += matrix(i)(j) * answers(j)
      }
      val rightSubLeft = matrix(i).last - left
      res = res :+ rightSubLeft
    }
    res
  }

  object ConverterToTriangleMatrix {

    var k: Integer = 0

    def swapColumns(matrix: Array[Array[Double]], firstCol: Int, secondCol: Int): Array[Array[Double]] = {
      var t: Double = 0
      var f = firstCol
      var sec = secondCol
      if (f > sec) {
        f = secondCol
        sec = firstCol
      }

      for (row <- matrix.indices) {
        for (column <- matrix(0).indices) {
          if (column == f) {
            t = matrix(row)(column)
          }
          if (column == sec) {
            matrix(row)(f) = matrix(row)(column)
            matrix(row)(column) = t
          }
        }
      }
      k += 1
      matrix
    }

    def checkSwapping(matrix: Array[Array[Double]], rowToSwap: Int, firstNonZeroColIdx: Int): Boolean = {
      val row = matrix(rowToSwap).dropRight(1)
      if (row.length == 1) false
      else true
    }

    def swapColWithMaxElCol(matrix: Array[Array[Double]], rowToSwap: Int, firstNonZeroColIdx: Int): Array[Array[Double]] = {
      val row = matrix(rowToSwap).dropRight(1)
      val indexMaxEl = row.zipWithIndex.maxBy(t => abs(t._1))._2
      indexMaxEl match {
        case ind if ind == firstNonZeroColIdx => matrix
        case _ => swapColumns(matrix, firstNonZeroColIdx, indexMaxEl)
      }
    }

    def toTriangle(matrix: Array[Array[Double]]): Array[Array[Double]] = {
      var changingMatrix = matrix
      for (mainRowIndex <- changingMatrix.indices by +1) {
        changingMatrix = swapColWithMaxElCol(changingMatrix, mainRowIndex, mainRowIndex)
        printMatrix(changingMatrix)
        for (rowIndex <- mainRowIndex + 1 until changingMatrix.length by +1) {
          changingMatrix = subOneRow(changingMatrix, mainRowIndex, rowIndex)
        }
      }
      changingMatrix
    }

    def subOneRow(matrix: Array[Array[Double]], nonChange: Int = 0, change: Int): Array[Array[Double]] = {
      var b = 0.0
      var a = 0.0
      val indexOfFirstNotZero = matrix(nonChange).indexWhere(num => num != 0)
      b = matrix(nonChange)(indexOfFirstNotZero)
      a = matrix(change)(indexOfFirstNotZero)
      for (i <- matrix(change).indices) {
        matrix(change)(i) -= matrix(nonChange)(i) * a / b
      }
      matrix
    }
  }

  object Determinant {
    def determinantOfTriangle(matrix: Array[Array[Double]], k: Int): Double = {
      var res = 1.0
      for (i <- matrix.indices) {
        res *= pow((-1), k) * matrix(i)(i)
      }
      res
    }


    def calculateDeterminant(matrix: Array[Array[Double]]): Double = {
      val n = matrix.length
      printMatrix(matrix, 2)
      if (n <= 0) throw new RuntimeException("N was 0 or less")
      if (n == 1) return matrix(0)(0)
      if (n == 2) {
        println(s"n=2: ${
          matrix(0)(0) * matrix(1)(1) - matrix(0)(1) * matrix(1)(0)
        }")
        return matrix(0)(0) * matrix(1)(1) - matrix(0)(1) * matrix(1)(0)
      }
      var i = 0
      var result: Double = 0
      var pow = 1
      while (i < n) {
        val cut = getMatrixWithoutRowAndColumn(matrix, 0, i)
        printMatrix(cut, 2)
        val el = matrix(0)(i)
        result = result + pow * el * calculateDeterminant(cut)
        println(s"result: $result")
        pow = -pow
        i += 1
      }
      result
    }

    def getMatrixWithoutRowAndColumn(matrix: Array[Array[Double]], ii: Int, jj: Int): Array[Array[Double]] = {
      var minor = Array[Array[Double]]()
      for (i <- matrix.indices) {
        if (i != ii) {
          var line = Array[Double]()
          for (j <- matrix(i).indices) {
            if (i != ii && j != jj) {
              line = line :+ matrix(i)(j)
            }
          }
          minor = minor :+ line
        }
      }
      minor
    }

    def getMinor(matrix: Array[Array[Double]], ii: Int, jj: Int): Double = {
      val m = getMatrixWithoutRowAndColumn(matrix, ii, jj)
      calculateDeterminant(m)
    }
  }

}