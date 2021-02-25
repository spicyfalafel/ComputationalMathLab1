import ru.itmo.FileHandler.{readFile, toMatrix}
import ru.itmo.Solver.Determinant.{calculateDeterminant, getMatrixWithoutRowAndColumn}

class DeterminantCheck extends UnitSpec {

  def initializeMatrix4x4: Array[Array[Double]] = {
    val matr4x4: Array[Array[Double]] = Array.ofDim[Double](4, 4)
    matr4x4(0)(0) = 1
    matr4x4(0)(1) = 2
    matr4x4(0)(2) = 3
    matr4x4(0)(3) = 4

    matr4x4(1)(0) = 1
    matr4x4(1)(1) = 2
    matr4x4(1)(2) = 3
    matr4x4(1)(3) = 4

    matr4x4(2)(0) = 1
    matr4x4(2)(1) = 2
    matr4x4(2)(2) = 3
    matr4x4(2)(3) = 4

    matr4x4(3)(0) = 1
    matr4x4(3)(1) = 2
    matr4x4(3)(2) = 3
    matr4x4(3)(3) = 4
    matr4x4
  }

  it should "work fine with 2x2 matrix" in {
    val matr2x2: Array[Array[Double]] = Array.ofDim[Double](2, 2)
    matr2x2(0)(0) = 1
    matr2x2(0)(1) = 2
    matr2x2(1)(0) = -1
    matr2x2(1)(1) = 3
    assert(5 == calculateDeterminant(matr2x2))
  }


  it should "work fine with 3x3 matrix" in {
    val matr3x3: Array[Array[Double]] = Array.ofDim[Double](3, 3)
    matr3x3(0)(0) = 1
    matr3x3(0)(1) = 2
    matr3x3(0)(2) = 2
    matr3x3(1)(0) = -1
    matr3x3(1)(1) = 3
    matr3x3(1)(2) = 3
    matr3x3(2)(0) = -1
    matr3x3(2)(1) = 3
    matr3x3(2)(2) = 3
    assert(0 == calculateDeterminant(matr3x3))
  }

}
