package ru.itmo

import java.nio.file.{Files, Paths}

import ru.itmo.MatrixKeyboardInput.getMatrixFromInput

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object Main {

  def printMatrix(matrix: Array[Array[Double]], decimals: Int): Unit = {
    println("Your matrix:")
    for(i <- matrix.indices){
      for(j <- matrix(i).indices){
        val el = BigDecimal(matrix(i)(j)).setScale(decimals, BigDecimal.RoundingMode.HALF_UP).toDouble
        print(el + "  ")
      }
      println()
    }
  }

  def main(args: Array[String]): Unit = {
    val mode = selectMode()
    val matrix: Array[Array[Double]] = getMatrix(mode)
    printMatrix(matrix, 2)
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
