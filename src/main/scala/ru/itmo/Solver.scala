package ru.itmo

import ru.itmo.MatrixKeyboardInput.{printMatrix, setScale}
import ru.itmo.Solver.ConverterToTriangleMatrix.{k, swappedColumnsMap, toTriangle}
import ru.itmo.Solver.Determinant.determinantOfTriangle

import scala.collection.mutable
import scala.math.{abs, pow}

object Solver {

  def solve(matrix: Array[Array[Double]]): Map[Int, Double] = {
    toTriangle(matrix)
    printMatrix(matrix)
    val determinant = determinantOfTriangle(matrix, k)
    println(s"determinant = $determinant")
    if (~=(determinant, 0, 1e-11)) {
      throw new Exception("No solutions")
    }
    getXVectorFromTriangleMatrix(matrix)
  }

  def getXVectorFromTriangleMatrix(triangleMatrix: Array[Array[Double]]): Map[Int, Double] = {
    var res: Map[Int, Double] = Map()
    val lastRowIndex = triangleMatrix.length - 1
    for (i <- lastRowIndex to 0 by -1) {
      val xi = getXFromRow(triangleMatrix(i), res)
      res = res + (i + 1 -> xi)
    }
    fixOrder(res.toSeq.sortBy(_._1).map(el => el._2).toArray)
  }

  def getXFromRow(line: Array[Double], xMap: Map[Int, Double]): Double = {
    val t = line.dropWhile(num => ~=(num, 0))
    val first = line.indexWhere(num => !(~=(num, 0)))
    if (first == -1) {
      throw new Exception("Infinite number of answer")
    }
    var x: Double = line.last
    if (t.length != 2) {
      for (i <- line.length - 2 to first + 1 by -1) {
        val xi: Double = xMap(i + 1)
        x -= xi * line(i)
      }
    }
    x / t(0)
  }

  def fixOrder(result: Array[Double]): Map[Int, Double] = {
    var res: Map[Int, Double] = Map()
    for (i <- result.indices by +1) {
      res = res + (swappedColumnsMap(i) -> result(i))
    }
    res
  }

  def checkAnswers(matrix: Array[Array[Double]], answers: Map[Int, Double]): Map[Int, Double] = {
    var res: Map[Int, Double] = Map()
    println("\n==Checking answers==")
    printMatrix(matrix)
    for (i <- matrix.indices) {
      var left: Double = 0
      for (j <- 0 to matrix(i).length - 2 by +1) {
        left += matrix(i)(j) * answers(j + 1)
        //print(s"${setScale(matrix(i)(j))} * ${setScale(answers(j + 1))} ")
      }
      val rightSubLeft = matrix(i).last - left
      println(s"b${i + 1} = $left, expected: ${matrix(i).last}")
      res += (i + 1 -> rightSubLeft)
    }
    res
  }

  def ~=(x: Double, y: Double, precision: Double = 1.0E-8): Boolean = {
    if ((x - y).abs < precision) true else false
  }

  object ConverterToTriangleMatrix {
    var k: Integer = 0
    //                                          x     column
    val swappedColumnsMap = new mutable.HashMap[Int, Int]()

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
      changeMap(firstCol, secondCol)
      k += 1
      matrix
    }

    def changeMap(firstColumn: Int, secondColumn: Int): Unit = {
      val t = swappedColumnsMap(firstColumn)
      swappedColumnsMap(firstColumn) = swappedColumnsMap(secondColumn)
      swappedColumnsMap(secondColumn) = t
    }

    def swapColWithMaxElCol(matrix: Array[Array[Double]],
                            rowToSwap: Int,
                            firstNonZeroColIdx: Int): Array[Array[Double]] = {
      val row = matrix(rowToSwap).dropRight(1)
      val indexMaxEl = row.zipWithIndex.maxBy(t => abs(t._1))._2
      indexMaxEl match {
        case ind if ind == firstNonZeroColIdx => matrix
        case _ => swapColumns(matrix, firstNonZeroColIdx, indexMaxEl)
      }
    }

    def initializeXMap(length: Int): Unit = {
      for (i <- 0 until length - 1) {
        swappedColumnsMap(i) = i + 1
      }
    }

    def checkIfRowIsNotZero(row: Array[Double]): Unit = {
      if (row.forall(num => ~=(num, 0))) throw new Exception("Infinite number of answer")
    }

    def toTriangle(matrix: Array[Array[Double]]): Array[Array[Double]] = {
      initializeXMap(matrix(0).length)
      var changingMatrix = matrix
      for (mainRowIndex <- changingMatrix.indices by +1) {
        changingMatrix = swapColWithMaxElCol(changingMatrix, mainRowIndex, mainRowIndex)
        printMatrix(changingMatrix)
        for (rowIndex <- mainRowIndex + 1 until changingMatrix.length by +1) {
          changingMatrix = subOneRow(changingMatrix, mainRowIndex, rowIndex)
          checkIfRowIsNotZero(matrix(rowIndex))
        }
      }
      changingMatrix
    }

    def subOneRow(matrix: Array[Array[Double]], nonChange: Int = 0, change: Int): Array[Array[Double]] = {
      var b = 0.0
      var a = 0.0
      val indexOfFirstNotZero = matrix(nonChange).indexWhere(num => !(~=(num, 0)))
      if (indexOfFirstNotZero == -1) {
        throw new Exception("Infinite number of answer")
      }
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