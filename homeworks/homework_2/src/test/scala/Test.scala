import utest._
import Exercises._

object Test extends TestSuite{
    val eps = java.lang.Math.pow(10, -6)
    val tests = Tests{
        'test_divBy3Or7 - {
            assert(divBy3Or7(1, 3) == Seq(3))
            assert(divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }

        'test_sumOfDivBy3Or5 - {
            assert(sumOfDivBy3Or5(1, 3) == 3)
            assert(sumOfDivBy3Or5(5, 9) == 20)
            assert(sumOfDivBy3Or5(0, 100) == 2418)
        }

        'test_primeFactor - {
            assert(primeFactor(10) == Seq(2, 5))
            assert(primeFactor(98) == Seq(2, 7))
            assert(primeFactor(300) == Seq(2, 3, 5))
        }

        'test_sumScalars - {
            assert(sumScalars(
              Vector2D(1.0, 0.0),
              Vector2D(0.0, 1.0),
              Vector2D(1.0, 0.0),
              Vector2D(0.0, 1.0)
            ) == 0.0)
            assert(Exercises.sumScalars(
              Vector2D(5.0, 3.0),
              Vector2D(3.0, -5.0),
              Vector2D(-3.0, 5.0),
              Vector2D(-5.0, 3.0)
            ) == 30.0)
            assert(sumScalars(
              Vector2D(7.5, 1.0),
              Vector2D(-1.0, -7.5),
              Vector2D(1.0, 2.5),
              Vector2D(5.0, 2.0)
            ) == -5.0)
        }

        'test_sumCosines - {
          assert(sumCosines(
            Vector2D(1.0, 0.0),
            Vector2D(0.0, 1.0),
            Vector2D(1.0, 0.0),
            Vector2D(0.0, 1.0)
          ) - 0.0 < eps)
          assert(sumCosines(
            Vector2D(5.0, 3.0),
            Vector2D(3.0, -5.0),
            Vector2D(-3.0, 5.0),
            Vector2D(-5.0, 3.0)
          ) - 30.0 / 34.0 < eps)
          assert(sumCosines(
            Vector2D(4.0, 3.0), Vector2D(3.0, 4.0),
            Vector2D(-4.0, 3.0), Vector2D(-3.0, 4.0)
          ) - 1.92 < eps)
        }

        'test_sortByHeavyweight - {
          assert(sortByHeavyweight() == Seq(
            "Tin", "Platinum", "Nickel", "Aluminum",
            "Titanium", "Lead", "Sodium", "Uranium",
            "Gold", "Tungsten", "Zirconium", "Chrome",
            "Iron", "Copper", "Silver", "Plutonium",
            "Cobalt", "Cesium", "Calcium", "Lithium",
            "Magnesium", "Potassium", "Graphite",
          ))
          assert(sortByHeavyweight(
            Map(
              "Titanium" ->  (2,   10.50),
              "Uranium" ->   (2,   19.04),
              "Magnesium" -> (10,  1.738),
              "Gold" ->     (2,   19.32),
            )
          ) == Seq(
              "Titanium", "Uranium", "Gold", "Magnesium",
          ))
        }
    }
}
