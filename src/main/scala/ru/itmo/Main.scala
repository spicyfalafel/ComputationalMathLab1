package ru.itmo

import java.nio.file.{Files, Paths}
import ru.itmo.MatrixKeyboardInput.{getMatrixFromInput, printMatrix, setScale}
import ru.itmo.Solver.ConverterToTriangleMatrix.{k, swappedColumnsMap, toTriangle}
import ru.itmo.Solver.Determinant.determinantOfTriangle
import ru.itmo.Solver.{checkAnswers, solve, ~=}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.StdIn.readLine

object Main {

  def main(args: Array[String]): Unit = {
    val mode = selectMode()
    val matrix: Array[Array[Double]] = getMatrix(mode)
    val initial = matrix.map(_.clone)
    printMatrix(matrix)
    try {
      val xVec = solve(matrix)
      println("x are:")
      for ((i, x) <- xVec) {
        println(s"x$i=$x ")
      }
      val pogr = checkAnswers(initial, xVec)
      println()
      for ((i, b) <- pogr) {
        println(s"r$i=${b}")
      }
    } catch {
      case e: Exception => println(e.printStackTrace())
    }
  }

  @tailrec
  def selectMode(): Int = {
    val mode: String = readLine("1 - file, 2 - input from keyboard: ")
    mode match {
      case "1" => 1
      case "2" => 2
      case _ => selectMode()
    }
  }

  @tailrec
  def askUserFileName(): String = {
    val filename = readLine("Enter filename:")
    if (Files.exists(Paths.get(filename))) {
      filename
    } else {
      askUserFileName()
    }
  }

  def getMatrixFromFile: Array[Array[Double]] = {
    val filename = askUserFileName()
    val file = FileHandler.readFile(filename)
    FileHandler.toMatrix(file)
  }

  def getMatrix(mode: Int): Array[Array[Double]] = {
    mode match {
      case 1 => getMatrixFromFile
      case 2 => getMatrixFromInput
    }
  }
}