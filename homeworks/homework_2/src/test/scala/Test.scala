import utest._

object Test extends TestSuite{

    val tests = Tests{
        test("test_divBy3Or7") {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }
        test("sumOfDivBy3Or5") {
            assert(Exercises.sumOfDivBy3Or5(1, 3) == 3)
            assert(Exercises.sumOfDivBy3Or5(5, 9) == 20)
            assert(Exercises.sumOfDivBy3Or5(0, 50) == 593)
        }
        test("primeFactor") {
            assert(Exercises.primeFactor(80) == Seq(2, 5))
            assert(Exercises.primeFactor(98) == Seq(2, 7))
            assert(Exercises.primeFactor(144) == Seq(2, 3))
        }
        val vec0 = Exercises.Vector2D(3, 7)
        val vec1 = Exercises.Vector2D(2, 5)
        val vec2 = Exercises.Vector2D(0, -1)
        val vec3 = Exercises.Vector2D(-3, -7)
        val vec4 = Exercises.Vector2D(3, 17)
        val vec5 = Exercises.Vector2D(-10, 6)
        test("sumScalars") {
            assert(Exercises.sumScalars(vec0, vec1, vec2, vec3) == 48)
            assert(Exercises.sumScalars(vec0, vec1, vec4, vec5) == 113)
            assert(Exercises.sumScalars(vec2, vec3, vec4, vec5) == 79)
        }
        test("sumCosines") {
            assert(Math.round(Exercises.sumCosines(vec0, vec1, vec2, vec3) * 100) / 100d == 1.92)
            assert(Math.round(Exercises.sumCosines(vec0, vec1, vec4, vec5) * 100) / 100d == 1.36)
            assert(Math.round(Exercises.sumCosines(vec2, vec3, vec4, vec5) * 100) / 100d == 1.28)
        }
        val ballArray1 = Map(
            "Gold" -> (3, 4d),
            "Silver" -> (5, 2d),
            "Bronze" -> (1, 3d),
            "Platinum" -> (2, 4.5)
        )
        val ballArray2 = Map(
            "Big" -> (10, 0.0000001),
            "Medium" -> (5, 2d),
            "Small" -> (1, 3d)
        )
        test("sortByHeavyweight") {
            assert(Exercises.sortByHeavyweight(ballArray1) == Seq("Bronze", "Platinum", "Gold", "Silver"))
            assert(Exercises.sortByHeavyweight(ballArray2) == Seq("Big", "Small", "Medium"))
        }
    }
}
