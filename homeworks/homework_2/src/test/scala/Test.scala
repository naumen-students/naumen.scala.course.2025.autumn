import utest._

object Test extends TestSuite {

    val tests = Tests{
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }
        'test_sumOfDivBy3Or5 - {
            assert(Exercises.sumOfDivBy3Or5(1,3) == 3)
            assert(Exercises.sumOfDivBy3Or5(1,10) == 33)
            assert(Exercises.sumOfDivBy3Or5(0,25) == 168)
        }
        'test_primeFactor - {
            assert(Exercises.primeFactor(168) == Seq(2,3,7))
            assert(Exercises.primeFactor(1099) == Seq(7,157))
            assert(Exercises.primeFactor(4114) == Seq(2,11,17))
        }
        'test_vector2d - {
            val v1 = Exercises.Vector2D(1, 0)
            val v2 = Exercises.Vector2D(0, 1)
            val v3 = Exercises.Vector2D(1, 1)
            val v4 = Exercises.Vector2D(2, 2)
            assert(Exercises.sumScalars(v1, v2, v3, v4) == 4.0)
            val result = Exercises.sumCosines(v1, v2, v3, v4)
            assert(math.abs(result - 1.0) < 1e-10)
        }
        'test_balls - {
            val balls: Map[String, (Int, Double)] =
                Map(
                    "Aluminum" -> (3, 2.6889), "Tungsten" -> (2, 19.35), "Graphite" -> (12, 2.1), "Iron" -> (3, 7.874),
                    "Gold" -> (2, 19.32), "Potassium" -> (14, 0.862), "Calcium" -> (8, 1.55), "Cobalt" -> (4, 8.90),
                    "Lithium" -> (12, 0.534), "Magnesium" -> (10, 1.738), "Copper" -> (3, 8.96), "Sodium" -> (5, 0.971),
                    "Nickel" -> (2, 8.91), "Tin" -> (1, 7.29), "Platinum" -> (1, 21.45), "Plutonium" -> (3, 19.25),
                    "Lead" -> (2, 11.336), "Titanium" -> (2, 10.50), "Silver" -> (4, 4.505), "Uranium" -> (2, 19.04),
                    "Chrome" -> (3, 7.18), "Cesium" -> (7, 1.873), "Zirconium" -> (3, 6.45)
                )
            assert(Exercises.sortByHeavyweight(balls) == Seq("Tin", "Platinum", "Nickel", "Aluminum", "Titanium", "Lead", "Sodium", "Uranium", "Gold", "Tungsten", "Zirconium", "Chrome", "Iron", "Copper", "Silver", "Plutonium", "Cobalt", "Cesium", "Calcium", "Lithium", "Magnesium", "Potassium", "Graphite"))
        }
    }
}
