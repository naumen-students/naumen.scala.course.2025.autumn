import utest._
import Exercises.primeFactor
import Exercises.Vector2D
import Exercises.scalar
import Exercises.cosBetween
import Exercises.sumScalars
import Exercises.sumCosines
import Exercises.sortByHeavyweight

object Test extends TestSuite{

    val tests = Tests{
        test("divBy3Or7") {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }

        test("sumOfDivBy3Or5")
        {
            assert(Exercises.sumOfDivBy3Or5(-5, -5) == -5L)
            assert(Exercises.sumOfDivBy3Or5(-3, -3) == -3L)
            assert(Exercises.sumOfDivBy3Or5(0, 0) == 0L)
            assert(Exercises.sumOfDivBy3Or5(3, 3) == 3L)
            assert(Exercises.sumOfDivBy3Or5(5, 5) == 5L)

            assert(Exercises.sumOfDivBy3Or5(-10, 0) == (-10L - 9L - 6L - 5L - 3L + 0L))
            assert(Exercises.sumOfDivBy3Or5(0, 10) == (0L + 3L + 5L + 6L + 9L + 10L))
            assert(Exercises.sumOfDivBy3Or5(-10, 10) == 0)

            assert(Exercises.sumOfDivBy3Or5(0, -10) == (-10L - 9L - 6L - 5L - 3L + 0L))
            assert(Exercises.sumOfDivBy3Or5(10, 0) == (0L + 3L + 5L + 6L + 9L + 10L))
            assert(Exercises.sumOfDivBy3Or5(10, -10) == 0)
        }

        test("primeFactor")
        {
            assert(primeFactor(-98) == Seq())
            assert(primeFactor(-80) == Seq())
            assert(primeFactor(-2) == Seq())
            assert(primeFactor(-1) == Seq())
            assert(primeFactor(0) == Seq())
            assert(primeFactor(1) == Seq())
            assert(primeFactor(2) == Seq(2))
            assert(primeFactor(80) == Seq(2, 5))
            assert(primeFactor(98) == Seq(2, 7))
        }

        test("sumScalars")
        {
            // Ненулевые векторы.
            val v1 = Vector2D(1, 1)
            val v2 = Vector2D(1, 2)
            val v3 = Vector2D(1, 3)
            val v4 = Vector2D(1, 4)
            assert(sumScalars(v1, v2, v3, v4) == (scalar(v1, v2) + scalar(v3, v4)))

            // Нулевые векторы.
            val v5 = Vector2D(0, 0)
            assert(sumScalars(v5, v5, v5, v5) == (scalar(v5, v5) + scalar(v5, v5)))

            // Ортогональные векторы.
            val v6 = Vector2D(10, 1)
            val v7 = Vector2D(-1, 10)
            val v8 = Vector2D(1, 10)
            val v9 = Vector2D(-10, 1)
            assert(sumScalars(v6, v7, v8, v9) == (scalar(v6, v7) + scalar(v8, v9)))

            // Параллельные векторы.
            val v10 = Vector2D(1, 2)
            val v11 = Vector2D(2, 4)
            val v12 = Vector2D(4, 8)
            val v13 = Vector2D(8, 16)
            assert(sumScalars(v10, v11, v12, v13) == (scalar(v10, v11) + scalar(v12, v13)))
        }

        test("sumCosines")
        {
            // Ненулевые векторы.
            val v1 = Vector2D(1, 1)
            val v2 = Vector2D(1, 2)
            val v3 = Vector2D(1, 3)
            val v4 = Vector2D(1, 4)
            assert(sumCosines(v1, v2, v3, v4) == (cosBetween(v1, v2) + cosBetween(v3, v4)))

            // Ортогональные векторы.
            val v5 = Vector2D(10, 1)
            val v6 = Vector2D(-1, 10)
            val v7 = Vector2D(1, 10)
            val v8 = Vector2D(-10, 1)
            assert(sumCosines(v5, v6, v7, v8) == (cosBetween(v5, v6) + cosBetween(v7, v8)))

            // Параллельные векторы.
            val v9 = Vector2D(1, 2)
            val v10 = Vector2D(2, 4)
            val v11 = Vector2D(4, 8)
            val v12 = Vector2D(8, 16)
            assert(sumCosines(v9, v10, v11, v12) == (cosBetween(v9, v10) + cosBetween(v11, v12)))
        }

        test("sortByHeavyweight")
        {
            val balls1 = Map(
                "Heavy" -> (1, 3.0),
                "Medium" -> (1, 2.0),
                "Light" -> (1, 1.0)
            )

            assert(sortByHeavyweight(balls1) == Seq("Light", "Medium", "Heavy"))

            val balls2 = Map(
                "Heavy" -> (3, 1.0),
                "Medium" -> (2, 1.0),
                "Light" -> (1, 1.0)
            )

            assert(sortByHeavyweight(balls1) == Seq("Light", "Medium", "Heavy"))
        }
    }
}
