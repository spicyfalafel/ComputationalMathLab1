package ru.itmo

import scala.io.Source

object FileHandler {

  def readFile(filename: String): Array[String] = {
    val bufferedSource = io.Source.fromFile(filename)
    val lines = (for (line <- bufferedSource.getLines()) yield line).toArray
    bufferedSource.close
    lines

  }

  def toMatrix(fileStrings: Array[String]): Array[Array[Double]] = {
    val h = fileStrings.length
    var matrix:Array[Array[Double]] = Array()
    for (i <- 0 until h by +1){
      val numbersInLine: Array[Double] = fileStrings(i).split("\\s+")
        .map(strDouble => strDouble.toDouble)
      matrix = matrix :+ numbersInLine
    }
    matrix
  }

}
