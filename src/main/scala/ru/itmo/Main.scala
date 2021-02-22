package ru.itmo

import java.nio.file.{Files, Paths}

import ru.itmo.MatrixKeyboardInput.{getMatrixFromInput, printMatrix}
import ru.itmo.Solver.ConverterToTriangleMatrix.{k, toTriangle}
import ru.itmo.Solver.Determinant.determinantOfTriangle
import ru.itmo.Solver.{checkAnswers, solve}

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object Main {

  def main(args: Array[String]): Unit = {
    val mode = selectMode()
    val matrix: Array[Array[Double]] = getMatrix(mode)
    printMatrix(matrix, 2)
    val triangle = toTriangle(matrix)
    printMatrix(triangle)
    val determinant = determinantOfTriangle(triangle, k)
    println(s"determinant = $determinant")
    val xVec = solve(triangle)
    println("x are:")
    for (i <- xVec.indices){
      print(s"x${i+1} = ${xVec(i)} ")
    }
    println("\n===================")
    val pogr = checkAnswers(triangle, xVec)
    for(i <- pogr.indices){
      print(s"b${i+1} = ${pogr(i)} ")
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
