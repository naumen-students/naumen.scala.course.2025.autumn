import utest._

object Test extends TestSuite{

    val tests = Tests{
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }

        'test_sumOfDivBy3Or5 - {
            assert(Exercises.sumOfDivBy3Or5(1, 5) == 8)
            assert(Exercises.sumOfDivBy3Or5(5, 9) == 20)
            assert(Exercises.sumOfDivBy3Or5(-3, 3) == 0)
            assert(Exercises.sumOfDivBy3Or5(-9, 3) == -20)
            assert(Exercises.sumOfDivBy3Or5(Int.MaxValue - 2, Int.MaxValue) == 2 * Int.MaxValue.toLong - 3)
        }

        'test_primeFactor - {
            assert(Exercises.primeFactor(-3) == Seq.empty[Int])
            assert(Exercises.primeFactor(0) == Seq.empty[Int])
            assert(Exercises.primeFactor(1) == Seq.empty[Int])
            assert(Exercises.primeFactor(2) == Seq(2))
            assert(Exercises.primeFactor(80) == Seq(2, 5))
            assert(Exercises.primeFactor(98) == Seq(2, 7))
        }

        'test_sumScalars - {
            assert(Exercises.sumScalars(Exercises.Vector2D(0, 0), Exercises.Vector2D(0, 0), Exercises.Vector2D(0, 0), Exercises.Vector2D(0, 0)) == 0)
            assert(Exercises.sumScalars(Exercises.Vector2D(0, 0), Exercises.Vector2D(0, 0), Exercises.Vector2D(1, 1), Exercises.Vector2D(1, 1)) == 2)
            assert(Exercises.sumScalars(Exercises.Vector2D(-1, -1), Exercises.Vector2D(1, 1), Exercises.Vector2D(1, 1), Exercises.Vector2D(1, -1)) == -2)
        }

        'test_sumCosines - {
            assert(Exercises.sumCosines(Exercises.Vector2D(1, 0), Exercises.Vector2D(0, 1), Exercises.Vector2D(0, 1), Exercises.Vector2D(1, 0)) == 0)
            assert(Exercises.sumCosines(Exercises.Vector2D(1, 0), Exercises.Vector2D(2, 0), Exercises.Vector2D(0, 1), Exercises.Vector2D(0, 3)) == 2)
            assert(Exercises.sumCosines(Exercises.Vector2D(1, 0), Exercises.Vector2D(-1, 0), Exercises.Vector2D(2, 2), Exercises.Vector2D(-3, -3)) == -2)
        }

        'test_sortByHeavyweight - {
            assert(Exercises.sortByHeavyweight(
                Map("Gold" -> (2, 19.32), "Tungsten" -> (2, 19.35), "Nickel" -> (2, 8.91)))
              == Seq("Nickel", "Gold", "Tungsten"))
            assert(Exercises.sortByHeavyweight(
                Map("Chrome" -> (3, 7.18), "Cesium" -> (7, 1.873), "Zirconium" -> (3, 6.45)))
              == Seq("Zirconium", "Chrome", "Cesium"))
            assert(Exercises.sortByHeavyweight(
                Map("Lithium" -> (12, 0.534), "Magnesium" -> (10, 1.738), "Copper" -> (3, 8.96), "Sodium" -> (5, 0.971)))
              == Seq("Sodium", "Copper", "Lithium", "Magnesium"))
        }
    }
}
