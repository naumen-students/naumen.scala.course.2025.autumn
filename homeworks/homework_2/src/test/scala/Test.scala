import utest._

object Test extends TestSuite{

    val tests = Tests{
        "test_divBy3Or7"- {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }

        "test_sumOfDivBy3Or5" - {
            assert(Exercises.sumOfDivBy3Or5(1, 5) == 8L)
            assert(Exercises.sumOfDivBy3Or5(0, 15) == 60L)
            assert(Exercises.sumOfDivBy3Or5(0, 100) == 2418L)
        }

        "test_primeFactor" - {
            assert(Exercises.primeFactor(80) == Seq(2, 5))
            assert(Exercises.primeFactor(98) == Seq(2, 7))
            assert(Exercises.primeFactor(-84) == Seq(2, 3, 7))
        }
        

        "test_sumScalars" - {
            var a = Exercises.Vector2D(1, 0)
            var b = Exercises.Vector2D(1, 0)
            var c = Exercises.Vector2D(0, 1)
            var d = Exercises.Vector2D(0, 1)
            assert(Exercises.sumScalars(a, b, c, d) == 2.0)

            a = Exercises.Vector2D(1, 0)
            b = Exercises.Vector2D(0, 1)
            c = Exercises.Vector2D(1, 0)
            d = Exercises.Vector2D(-1, 0)
            assert(Exercises.sumScalars(a, b, c, d) == -1.0)

        }

        "test_sumCosines" - {
            val a = Exercises.Vector2D(1, 2)
            val b = Exercises.Vector2D(3, 4)
            val c = Exercises.Vector2D(2, 3)
            val d = Exercises.Vector2D(4, 5)

            val cos1 = 11.0 / (math.sqrt(1 * 1 + 2 * 2) * math.sqrt(3 * 3 + 4 * 4))
            val cos2 = 23.0 / (math.sqrt(2 * 2 + 3 * 3) * math.sqrt(4 * 4 + 5 * 5))
            val expected = cos1 + cos2
            assert(math.abs(Exercises.sumCosines(a, b, c, d) - expected) == 0.0)

        }

        "test_sortByHeavyweight" - {
            val testBalls = Map("A" -> (1, 1.0), "B" -> (2, 1.0), "C" -> (2, 0.5))
            assert(Exercises.sortByHeavyweight(testBalls) == Seq("A", "C", "B"))

            val res = Exercises.sortByHeavyweight()
            assert(res.indexOf("Nickel") < res.indexOf("Lead"))
            assert(res.indexOf("Uranium") < res.indexOf("Gold"))
        }
    }
}
