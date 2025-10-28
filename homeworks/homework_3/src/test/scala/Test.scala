import utest._
import homework_3._

object TableTests extends TestSuite {
  val tests = Tests {
    test("basic") {
      val t = new Table(2, 2)
      t.getCell(0, 0).get.toString ==> "empty"
      t.setCell(0, 0, new NumberCell(123))
      t.getCell(0, 0).get.toString ==> "123"
    }

    test("reference") {
      val t = new Table(2, 2)
      t.setCell(0, 0, new NumberCell(5))
      t.setCell(1, 0, new ReferenceCell(0, 0, t))
      t.getCell(1, 0).get.toString ==> "5"
    }

    test("out of range") {
      val t = new Table(1, 1)
      t.setCell(0, 0, new ReferenceCell(5, 5, t))
      t.getCell(0, 0).get.toString ==> "outOfRange"
    }

    test("cyclic") {
      val t = new Table(2, 1)
      val c1 = new ReferenceCell(1, 0, t)
      val c2 = new ReferenceCell(0, 0, t)
      t.setCell(0, 0, c1)
      t.setCell(1, 0, c2)
      t.getCell(0, 0).get.toString ==> "cyclic"
    }
  }
}
