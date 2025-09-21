import utest._

object Test extends TestSuite{

    val tests = Tests{
        test("divBy3Or7") {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }

        test("sumOfDivBy3Or5") {
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
    }
}
