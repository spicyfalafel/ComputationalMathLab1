import collection.mutable.Stack
import ru.itmo.FileHandler

class CheckFileHandler extends UnitSpec {
  lazy val file4x4 = "4x4"

  "File reader" should "read whole file and return array[array[double]]" in {
    assert(FileHandler.readFile(file4x4).size == 4)
  }

  it should "toMatrix should be of size 4x4" in {
    val strings = Array("1 2 3 4", "4 3 2 1", "1 2 3 4", "4 3 2 1")
    val matr = FileHandler.toMatrix(strings)
    assert(matr.length == 4 && matr(0).length == 4)
  }
}
