
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