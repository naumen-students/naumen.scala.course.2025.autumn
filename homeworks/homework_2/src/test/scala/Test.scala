import utest._
import Exercises._

object Test extends TestSuite{

    val tests = Tests{
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }

        'test_sumOfDivBy3Or5 - {
            assert(sumOfDivBy3Or5(-2, 2) == 0)
            assert(sumOfDivBy3Or5(-2, 4) == 3)
            assert(sumOfDivBy3Or5(0, 3) == 3)
            assert(sumOfDivBy3Or5(4, 5) == 5)
            assert(sumOfDivBy3Or5(-3, 0) == -3)
            assert(sumOfDivBy3Or5(-5, -4) == -5)
            assert(sumOfDivBy3Or5(0, 5) == 8)
            assert(sumOfDivBy3Or5(-5, 0) == -8)
            assert(sumOfDivBy3Or5(-5, 5) == 0)
        }

        'test_primeFactor - {
            assert(primeFactor(0) == Seq.empty)
            assert(primeFactor(1) == Seq.empty)
            assert(primeFactor(2) == Seq(2))
            assert(primeFactor(4) == Seq(2))
            assert(primeFactor(6) == Seq(2,3))
            assert(primeFactor(12) == Seq(2,3))
            assert(primeFactor(30) == Seq(2,3,5))
            assert(primeFactor(80) == Seq(2,5))
            assert(primeFactor(-1) == Seq.empty)
            assert(primeFactor(-2) == Seq(2))
            assert(primeFactor(-4) == Seq(2))
            assert(primeFactor(-6) == Seq(2,3))
            assert(primeFactor(-12) == Seq(2,3))
            assert(primeFactor(-30) == Seq(2,3,5))
        }

      'test_sumScalars - {
        assert(sumScalars(Vector2D(0, 0), Vector2D(0, 0), Vector2D(0, 0), Vector2D(0, 0)) == 0)
        assert(sumScalars(Vector2D(1, 1), Vector2D(1, 1), Vector2D(1, 1), Vector2D(1, 1)) == 4)
        assert(sumScalars(Vector2D(1, 0), Vector2D(0, 1), Vector2D(1, 1), Vector2D(1, 1)) == 2)
        assert(sumScalars(Vector2D(-1, -1), Vector2D(1, 1), Vector2D(1, 1), Vector2D(-1, -1)) == -4)
        assert(sumScalars(Vector2D(1.5, -2), Vector2D(2, 1.5), Vector2D(1.5, 2), Vector2D(1.5, 4)) == 10.25)
      }

      'test_sumCosines - {
        assert(sumCosines(Vector2D(1, 0), Vector2D(0, 1), Vector2D(0, 1), Vector2D(1, 0)) == 0)
        assert(sumCosines(Vector2D(1, 0), Vector2D(1, 0), Vector2D(0, 1), Vector2D(0, 1)) == 2)
        assert(sumCosines(Vector2D(1, 0), Vector2D(-1, 0), Vector2D(0, 1), Vector2D(0, -1)) == -2)
        assert(sumCosines(Vector2D(1, 0), Vector2D(-1, 0), Vector2D(0, 1), Vector2D(0, -1)) == -2)
        assert(sumCosines(Vector2D(1, 0), Vector2D(1, 1), Vector2D(1, 1), Vector2D(0, 1)) - Math.sqrt(2) < 0.0001)
      }

      'test_sortByHeavyweight - {
        assert(sortByHeavyWeight(Map.empty) == Seq.empty)
        assert(sortByHeavyWeight(Map("Name" -> (1, 1))) == Seq("Name"))
        assert(sortByHeavyWeight(Map("Name1" -> (1, 1), "Name2" -> (1, 2))) == Seq("Name1", "Name2"))
        assert(sortByHeavyWeight(Map("Name1" -> (2, 3), "Name2" -> (1, 3))) == Seq("Name2", "Name1"))
        assert(sortByHeavyWeight(balls) == Seq(
          "Tin",
          "Platinum",
          "Nickel",
          "Aluminum",
          "Titanium",
          "Lead",
          "Sodium",
          "Uranium",
          "Gold",
          "Tungsten",
          "Zirconium",
          "Chrome",
          "Iron",
          "Copper",
          "Silver",
          "Plutonium",
          "Cobalt",
          "Cesium",
          "Calcium",
          "Lithium",
          "Magnesium",
          "Potassium",
          "Graphite"
        ))
      }
    }
}
