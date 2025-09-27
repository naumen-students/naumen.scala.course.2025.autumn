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
            assert(sumOfDivBy3Or5(1, 5) == 8)
            assert(sumOfDivBy3Or5(5, 10) == 30)
            assert(sumOfDivBy3Or5(5, 5) == 5)
        }
        'test_primeFactor - {
            assert(primeFactor(1) == Seq.empty)
            assert(primeFactor(2) == Seq(2))
            assert(primeFactor(24) == Seq(2, 3))
            assert(primeFactor(98) == Seq(2, 7))
        }
        'test_sumScalars - {
            assert(sumScalars(Vector2D(1, 1), Vector2D(1, 1), Vector2D(1, 1), Vector2D(1, 1)) == 4)
            assert(sumScalars(Vector2D(1, 0), Vector2D(1, 0), Vector2D(0, 1), Vector2D(0, 1)) == 2)
        }
        'test_sumCosines - {
            assert(sumCosines(Vector2D(1, 0), Vector2D(0, 1), Vector2D(1, 0), Vector2D(1, 0)) == 1)
            assert((sumCosines(Vector2D(1, 1), Vector2D(1, -1), Vector2D(-1, 1), Vector2D(1, 1)) - 0d).abs < 1e-6)
        }
        'test_sortByHeavyweight - {
            assert(sortByHeavyweight(Map.empty) == Seq.empty)
            assert(sortByHeavyweight(Map("1" -> (1 -> 1))) == Seq("1"))
            assert(sortByHeavyweight(Map("1" -> (1 -> 1), "2" -> (1 -> 2))) == Seq("1", "2"))
            assert(sortByHeavyweight(Map("2" -> (2 -> 1), "1" -> (1 -> 1))) == Seq("1", "2"))
        }
    }
}