import utest._

object Test extends TestSuite{

    val tests = Tests{
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }
        'test_sumOfDivBy3Or5 - {
            assert(Exercises.sumOfDivBy3Or5(1, 8) == 14)
            assert(Exercises.sumOfDivBy3Or5(-10, -1) == -33)
            assert(Exercises.sumOfDivBy3Or5(-5, 5) == 0)
            assert(Exercises.sumOfDivBy3Or5(1, 2) == 0)
            assert(Exercises.sumOfDivBy3Or5(3, 3) == 3)
            assert(Exercises.sumOfDivBy3Or5(5, 2) == 0)
            val bigNum = math.pow(2, 30).toInt // 1 073 741 824
            assert(Exercises.sumOfDivBy3Or5(bigNum, bigNum + 3) == bigNum.toLong + 1 + bigNum + 2)
        }
        'test_primeFactor - {
            assert(Exercises.primeFactor(2) == Seq(2))
            assert(Exercises.primeFactor(997) == Seq(997))
            assert(Exercises.primeFactor(80) == Seq(2, 5))
            assert(Exercises.primeFactor(39215) == Seq(5, 11, 23, 31))
            assert(Exercises.primeFactor(math.pow(3, 10).toInt) == Seq(3))
            intercept[IllegalArgumentException](Exercises.primeFactor(1))
        }
        'test_sumScalarsSumCosines - {
            val simpleVec0 = Exercises.Vector2D(1, 1)
            val simpleVec1 = Exercises.Vector2D(2, 2)
            val simpleVec2 = Exercises.Vector2D(3, 3)
            val simpleVec3 = Exercises.Vector2D(4, 4)
            val zeroVec = Exercises.Vector2D(0, 0)
            val negativeVec0 = Exercises.Vector2D(-1, -1)
            val negativeVec1 = Exercises.Vector2D(-2, -2)
            val orthogonalVec0 = Exercises.Vector2D(1, 0)
            val orthogonalVec1 = Exercises.Vector2D(0, 1)
            val orthogonalVec2 = Exercises.Vector2D(2, 3)
            val orthogonalVec3 = Exercises.Vector2D(-3, 2)

            val simpleVecSumScalar = Exercises.sumScalars(simpleVec0, simpleVec1, simpleVec2, simpleVec3)
            val simpleVecSumCos = Exercises.sumCosines(simpleVec0, simpleVec1, simpleVec2, simpleVec3)
            assertApprox(simpleVecSumScalar, 28)
            assertApprox(simpleVecSumCos, 2)

            val zeroVecSumScalar = Exercises.sumScalars(zeroVec, simpleVec0, simpleVec1, zeroVec)
            val zeroVecSumCos = Exercises.sumCosines(zeroVec, simpleVec0, simpleVec1, zeroVec)
            assertApprox(zeroVecSumScalar, 0)
            assert(zeroVecSumCos.isNaN)

            val negativeVecSumScalar = Exercises.sumScalars(negativeVec0, simpleVec0, negativeVec1, simpleVec1)
            val negativeVecSumCos = Exercises.sumCosines(negativeVec0, simpleVec0, negativeVec1, simpleVec1)
            assertApprox(negativeVecSumScalar, -10)
            assertApprox(negativeVecSumCos, -2)

            val orthogonalVecSumScalar =
              Exercises.sumScalars(orthogonalVec0, orthogonalVec1, orthogonalVec2, orthogonalVec3)
            val orthogonalVecSumCos =
              Exercises.sumCosines(orthogonalVec0, orthogonalVec1, orthogonalVec2, orthogonalVec3)
            assertApprox(orthogonalVecSumScalar, 0)
            assertApprox(orthogonalVecSumCos, 0)
        }
        'test_sortByHeavyweight - {
            assert(Exercises.sortByHeavyweight(Map()) == Seq())
            assert(Exercises.sortByHeavyweight(Map("Test" -> (1, 1))) == Seq("Test"))

            val balls = Map("First" -> (1, 1.1), "Second" -> (2, 1.1), "Third" -> (1, 2.2))
            assert(Exercises.sortByHeavyweight(balls) == Seq("First", "Third", "Second"))
        }
    }

    def assertApprox(actual: Double, expected: Double, epsilon: Double = 1e-9): Unit =
      assert(math.abs(actual - expected) < epsilon)
}
