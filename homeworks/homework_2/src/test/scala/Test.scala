import utest._

object Test extends TestSuite {

    val tests = Tests {
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }

        'test_sumOfDivBy3Or5 - {
            assert(Exercises.sumOfDivBy3Or5(1, 10) == 33)
            assert(Exercises.sumOfDivBy3Or5(1, 2) == 0)
            assert(Exercises.sumOfDivBy3Or5(1, 5) == 8)

            assert(Exercises.sumOfDivBy3Or5(0, 0) == 0)
            assert(Exercises.sumOfDivBy3Or5(15, 15) == 15)

            assert(Exercises.sumOfDivBy3Or5(-10, 10) == 0)
        }

        'test_primeFactor - {
            assert(Exercises.primeFactor(80) == Seq(2, 5))
            assert(Exercises.primeFactor(98) == Seq(2, 7))

            assert(Exercises.primeFactor(17) == Seq(17))
            assert(Exercises.primeFactor(13) == Seq(13))

            assert(Exercises.primeFactor(36) == Seq(2, 3)) // 2*2*3*3
            assert(Exercises.primeFactor(100) == Seq(2, 5)) // 2*2*5*5

            assert(Exercises.primeFactor(1) == Seq.empty)
            assert(Exercises.primeFactor(0) == Seq.empty)
            assert(Exercises.primeFactor(-10) == Seq.empty)
        }

        'test_vector_operations - {
            val vec1 = Exercises.Vector2D(1, 0)
            val vec2 = Exercises.Vector2D(0, 1)
            val vec3 = Exercises.Vector2D(1, 1)
            val vec4 = Exercises.Vector2D(2, 2)


            assert(Exercises.sumScalars(vec1, vec2, vec3, vec4) == 4.0) // 0 + 4 = 4
            assert(Exercises.sumScalars(vec1, vec1, vec2, vec2) == 2.0) // 1 + 1 = 2

            assert(math.abs(Exercises.sumCosines(vec1, vec2, vec3, vec4) - 1.0) < 0.0001) // 0 + 1 = 1
            assert(Exercises.sumCosines(vec1, vec1, vec2, vec2) == 2.0) // 1 + 1 = 2
        }

        'test_sortByHeavyweight - {
            val testBalls = Map(
                "Small" -> (1, 1.0), // масса ~4.19
                "Medium" -> (2, 1.0), // масса ~33.51
                "Large" -> (3, 1.0) // масса ~113.1
            )

            val result = Exercises.sortByHeavyweight(testBalls)
            assert(result == Seq("Small", "Medium", "Large"))

            val testBalls2 = Map(
                "Light" -> (2, 1.0), // масса ~33.51
                "Heavy" -> (1, 10.0) // масса ~41.89
            )
            val result2 = Exercises.sortByHeavyweight(testBalls2)
            assert(result2 == Seq("Light", "Heavy"))
            val originalResult = Exercises.sortByHeavyweight()
            assert(originalResult.nonEmpty)
            assert(originalResult.head == "Tin")
            assert(originalResult.contains("Lithium"))
            assert(originalResult.last == "Graphite")
        }
    }
}


