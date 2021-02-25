import ru.itmo.Solver.Determinant.calculateDeterminant
import ru.itmo.Solver.solve

class SolveCheck extends UnitSpec {

  /*
  x + 2y = 4
  2x + y = 5
   */
  "Solver" should "work fine with 2x2 matrix" in {
    val matr2x2: Array[Array[Double]] = Array.ofDim[Double](2, 3)
    matr2x2(0)(0) = 1
    matr2x2(0)(1) = 2
    matr2x2(1)(0) = 2
    matr2x2(1)(1) = 1
    matr2x2(0)(2) = 4
    matr2x2(1)(2) = 5
    val res = solve(matr2x2)
    assert(res.values.size == 2)
    println(res.mkString("Array(", ", ", ")"))
  }
}
