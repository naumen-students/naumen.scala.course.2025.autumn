import utest._

object Test extends TestSuite{

  val tests = Tests{
    'test_divBy3Or7 - {
      assert(Exercises.divBy3Or7(1, 3) == Seq(3))
      assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
      assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
    }
    'test_sumOfDivBy3Or5 - {
      assert(Exercises.sumOfDivBy3Or5(5, 0) == 0)
      assert(Exercises.sumOfDivBy3Or5(3, 3) == 3)
      assert(Exercises.sumOfDivBy3Or5(5, 5) == 5)
      assert(Exercises.sumOfDivBy3Or5(1, 1) == 0)
      assert(Exercises.sumOfDivBy3Or5(1, 10) == 33)
      assert(Exercises.sumOfDivBy3Or5(-10, 0) == -33)
      assert(Exercises.sumOfDivBy3Or5(1, 1000) == 234168)
    }
    'primeFactor - {
      assert(Exercises.primeFactor(80) == Seq(2,5))
      assert(Exercises.primeFactor(98) == Seq(2,7))
      assert(Exercises.primeFactor(1) == Seq.empty)
      assert(Exercises.primeFactor(17) == Seq(17))
      assert(Exercises.primeFactor(27) == Seq(3))
      assert(Exercises.primeFactor(2310) == Seq(2, 3, 5, 7, 11))
      assert(Exercises.primeFactor(-10) == Seq.empty)
    }

    'sumScalars - {
      assert(Exercises.sumScalars(Exercises.Vector2D(1, 2),Exercises.Vector2D(3, 4),Exercises.Vector2D(5, 6),Exercises.Vector2D(7, 8)) == 94.0)
      assert(Exercises.sumScalars(Exercises.Vector2D(0, 0),Exercises.Vector2D(0, 0),Exercises.Vector2D(0, 0),Exercises.Vector2D(0, 0)) == 0.0)
    }
    'sumCosines - {
      assert(Exercises.sumCosines(Exercises.Vector2D(1, 0),Exercises.Vector2D(2, 0),Exercises.Vector2D(0, 1),Exercises.Vector2D(0, 3)) == 2.0)
      assert(Exercises.sumCosines(Exercises.Vector2D(1, 0),Exercises.Vector2D(0, 1),Exercises.Vector2D(0, 1),Exercises.Vector2D(1, 0)) == 0.0)
    }
    'sortByHeavyweight - {
      val testBalls = Map(
        "Light" -> (1, 1.0),
        "Medium" -> (1, 2.0),
        "Heavy" -> (2, 1.0)
      )

      assert(Exercises.sortByHeavyweight(testBalls) == Seq("Light", "Medium", "Heavy"))
      assert(Exercises.sortByHeavyweight(Map.empty) == Seq.empty)

    }
  }
}