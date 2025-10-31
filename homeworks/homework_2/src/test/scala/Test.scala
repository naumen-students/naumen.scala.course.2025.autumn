import utest._
import Exercises._

object Test extends TestSuite {

    val tests = Tests {

        // ---------- ПРИМЕР ----------
        'divBy3Or7 - {
            assert(divBy3Or7(1, 3) == Seq(3))
            assert(divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(divBy3Or7(0, 20) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18))
        }

        // ---------- ЗАДАНИЕ I ----------
        'sumOfDivBy3Or5 - {
            assert(sumOfDivBy3Or5(1, 10) == 33)
            assert(sumOfDivBy3Or5(0, 15) == 60)
            assert(sumOfDivBy3Or5(10, 15) == 37)
        }

        // ---------- ЗАДАНИЕ II ----------
        'primeFactor - {
            assert(primeFactor(80) == Seq(2, 5))
            assert(primeFactor(98) == Seq(2, 7))
            assert(primeFactor(13) == Seq(13))
            assert(primeFactor(1).isEmpty)
        }

        // ---------- ЗАДАНИЕ III ----------
        'sumScalarsAndSumCosines - {
            val a = Vector2D(1, 0)
            val b = Vector2D(0, 1)
            val c = Vector2D(2, 2)
            val d = Vector2D(3, 0)

            assert(sumScalars(a, b, c, d) == 6)

            val res = sumCosines(a, b, c, d)
            assert(math.abs(res - 0.7071) < 1e-3)
        }

        // ---------- ЗАДАНИЕ IV ----------
        'sortByHeavyweight - {
            val smallSet = Map(
                "A" -> (1, 1.0),
                "B" -> (2, 1.0),
                "C" -> (1, 2.0)
            )
            assert(sortByHeavyweight(smallSet) == Seq("A", "C", "B"))
        }
    }
}
