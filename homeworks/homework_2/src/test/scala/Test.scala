import utest._

object Test extends TestSuite{

    val tests = Tests{
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
            assert(Exercises.divBy3Or7(-3, 7) == Seq(-3, 0, 3, 6, 7))
            assert(Exercises.divBy3Or7(7, 7) == Seq(7))
            assert(Exercises.divBy3Or7(2, 2) == Seq())
            assert(Exercises.divBy3Or7(5, 3) == Seq())
        }

        'test_sumOfDivBy3Or5 - {
            assert(Exercises.sumOfDivBy3Or5(1, 5) == 8)
            assert(Exercises.sumOfDivBy3Or5(3, 25) == 168)
            assert(Exercises.sumOfDivBy3Or5(-5, -1) == -8)
            assert(Exercises.sumOfDivBy3Or5(-5, 5) == 0)
            assert(Exercises.sumOfDivBy3Or5(0, 0) == 0)
            assert(Exercises.sumOfDivBy3Or5(1, 3) == 3)
            assert(Exercises.sumOfDivBy3Or5(-5, -4) == -5)
            assert(Exercises.sumOfDivBy3Or5(5, 5) == 5)
            assert(Exercises.sumOfDivBy3Or5(2, 2) == 0)
            assert(Exercises.sumOfDivBy3Or5(5, 3) == 0)
        }

        'test_primeFactor - {
            assert(Exercises.primeFactor(1) == Seq())
            assert(Exercises.primeFactor(3) == Seq(3))
            assert(Exercises.primeFactor(-5) == Seq())
            assert(Exercises.primeFactor(80) == Seq(2, 5))
            assert(Exercises.primeFactor(98) == Seq(2, 7))
            assert(Exercises.primeFactor(17) == Seq(17))
            assert(Exercises.primeFactor(27) == Seq(3))
            assert(Exercises.primeFactor(30) == Seq(2, 3, 5))
        }

        'test_sumScalars - {
            val zero = Exercises.Vector2D(0, 0)
            val normal = Exercises.Vector2D(1, 1)
            assert(Exercises.sumScalars(
                Exercises.Vector2D(2, 3), 
                Exercises.Vector2D(4, 5), 
                Exercises.Vector2D(6, 7), 
                Exercises.Vector2D(8, 9)
            ) == 134.0)
            assert(Exercises.sumScalars(
                Exercises.Vector2D(-2, -3), 
                Exercises.Vector2D(-4, -5), 
                Exercises.Vector2D(-6, -7), 
                Exercises.Vector2D(-8, -9)
            ) == 134.0)
            assert(Exercises.sumScalars(zero, zero, zero, zero) == 0.0)
            assert(Exercises.sumScalars(zero, normal, normal, zero) == 0.0)
        }

        'test_sumCosines - {
            val zero = Exercises.Vector2D(0, 0)
            assert(math.abs(Exercises.sumCosines(
                Exercises.Vector2D(1, 0), 
                Exercises.Vector2D(0, 1), 
                Exercises.Vector2D(1, 1), 
                Exercises.Vector2D(1, 0)
            ) - 0.707) < 0.001)
            assert(math.abs(Exercises.sumCosines(
                Exercises.Vector2D(1, 1), 
                Exercises.Vector2D(-1, -1), 
                Exercises.Vector2D(1, 1), 
                Exercises.Vector2D(-1, -1)
                ) + 2.0) < 0.001)
            assert(math.abs(Exercises.sumCosines(
                Exercises.Vector2D(1, 0), 
                Exercises.Vector2D(0, 1), 
                Exercises.Vector2D(0, 1), 
                Exercises.Vector2D(1, 0)
                )) < 0.001)
        }

        'test_sortByHeavyweight - {
            assert(Exercises.sortByHeavyweight() == Seq(
                "Tin", "Platinum", "Nickel", "Aluminum", "Titanium", "Lead", 
                "Sodium", "Uranium", "Gold", "Tungsten", "Zirconium", "Chrome", 
                "Iron", "Copper", "Silver", "Plutonium", "Cobalt", "Cesium", 
                "Calcium", "Lithium", "Magnesium", "Potassium", "Graphite"
            ))
            assert(Exercises.sortByHeavyweight(Map()) == Seq())
            assert(Exercises.sortByHeavyweight(Map("Any" -> (10, 5.0))) == Seq("Any"))
            assert(Exercises.sortByHeavyweight(Map("Small" -> (1, 1.0),  "Big" -> (1, 100.0))) == Seq("Small", "Big"))
        }
    }
}
