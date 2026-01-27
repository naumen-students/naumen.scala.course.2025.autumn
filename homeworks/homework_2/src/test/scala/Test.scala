import Exercises.Vector2D
import utest._

object Test extends TestSuite{
  implicit class DoubleCompare(x: Double) {
    def ~=(y: Double, precision: Double = 1e-6): Boolean = (x - y).abs < precision
  }

  val tests = Tests{
    test("Exercises.divBy3Or7") {
      assert(Exercises.divBy3Or7(1, 3) == Seq(3))
      assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
      assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
    }

    test("Exercises.sumOfDivBy3Or5") {
      assert(Exercises.sumOfDivBy3Or5(1, 3) == 3)
      assert(Exercises.sumOfDivBy3Or5(5, 9) == 20)
      assert(Exercises.sumOfDivBy3Or5(0, 100) == 2418)

      assert(Exercises.sumOfDivBy3Or5(0, 2) == 0)
      assert(Exercises.sumOfDivBy3Or5(1, 2) == 0)
    }

    test("Exercises.primeFactor") {
      assert(Exercises.primeFactor(100) == Seq(2, 5))
      assert(Exercises.primeFactor(30) == Seq(2, 3, 5))
      assert(Exercises.primeFactor(25) == Seq(5))
      assert(Exercises.primeFactor(97) == Seq(97))

      assert(Exercises.primeFactor(1) == Seq())
      assert(Exercises.primeFactor(0) == Seq())
      assert(Exercises.primeFactor(-1) == Seq())
    }

    test("Exercises.sumScalars") {
      assert(Exercises.sumScalars(Vector2D(2.3, -0.1), Vector2D(-5.6, 11),
                                  Vector2D(1.45, 4.33), Vector2D(-42, -1.15)) ~= -79.8595)
      assert(Exercises.sumScalars(Vector2D(2.3, -0.1), Vector2D(-5.6, 11),
                                  Vector2D(1.45, 4.33), Vector2D(0, 0)) ~= -13.98)
      assert(Exercises.sumScalars(Vector2D(2.3, -0.1), Vector2D(-5.6, 11),
                                  Vector2D(1.45, 4.33), Vector2D(-4.33, 1.45)) ~= -13.98)
    }

    test("Exercises.sumCosines") {
      assert(Exercises.sumCosines(Vector2D(2.3, -0.1), Vector2D(-5.6, 11),
                                  Vector2D(1.45, 4.33), Vector2D(-42, -1.15)) ~= -0.835341)
      assert(Exercises.sumCosines(Vector2D(2.3, -0.1), Vector2D(-5.6, 11),
                                  Vector2D(1.45, 4.33), Vector2D(0.1, -0.1)) ~= -0.937939)
    }

    test("Exercises.sortByHeavyweight") {
      assert(Exercises.sortByHeavyweight() == Seq("Tin", "Platinum", "Nickel", "Aluminum", "Titanium", "Lead",
                                                  "Sodium", "Uranium", "Gold", "Tungsten", "Zirconium", "Chrome",
                                                  "Iron", "Copper", "Silver", "Plutonium", "Cobalt", "Cesium",
                                                  "Calcium", "Lithium", "Magnesium", "Potassium", "Graphite"))
      assert(Exercises.sortByHeavyweight(Map[String, (Int, Double)]("Lithium" ->  (12,  0.534))) == Seq[String]("Lithium"))
      assert(Exercises.sortByHeavyweight(Map[String, (Int, Double)]()) == Seq[String]())
    }
  }
}
