import utest._

object Test extends TestSuite{

    val tests = Tests{
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }

      'sumOfDivBy3Or5 - {
        assert(Exercises.sumOfDivBy3Or5(1, 17) == 60)
        assert(Exercises.sumOfDivBy3Or5(1, 100) == 2418)
        assert(Exercises.sumOfDivBy3Or5(0, 2) == 0)
      }

      'primeFactor - {
        assert(Exercises.primeFactor(80) == Seq(2, 5))
        assert(Exercises.primeFactor(98) == Seq(2, 7))
        assert(Exercises.primeFactor(1) == Seq())
        assert(Exercises.primeFactor(120) == Seq(2, 3, 5))
      }

      'sumScalars - {
        val firstVector = Exercises.Vector2D(1, 2)
        val secondVector = Exercises.Vector2D(3, 4)
        val thirdVector = Exercises.Vector2D(1, 0)
        val fourthVector = Exercises.Vector2D(0, 1)
        assert(Exercises.sumScalars(firstVector, secondVector, thirdVector, fourthVector) == 11.0)
      }

      'sumCosines - {
        val firstVector = Exercises.Vector2D(1, 1)
        val secondVector = Exercises.Vector2D(2, 0)
        val thirdVector = Exercises.Vector2D(1, 0)
        val fourthVector = Exercises.Vector2D(0, 1)
        assert(math.abs(Exercises.sumCosines(firstVector, secondVector, thirdVector, fourthVector)) - 0.7071067 < 1e-6)
      }

      'sortByHeavyweight - {
        val balls = Map(
          "Light" -> (1, 1.0),
          "Medium" -> (2, 1.0),
          "Heavy" -> (2, 10.0)
        )
        assert(Exercises.sortByHeavyweight(balls) == Seq("Light", "Medium", "Heavy"))
        val oneBall = Map("OnlyOne" -> (5, 2.0))
        assert(Exercises.sortByHeavyweight(oneBall) == Seq("OnlyOne"))
      }
    }
}
