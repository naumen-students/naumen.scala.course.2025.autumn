import utest._

object Test extends TestSuite {
  val tests = Tests {
    'test_divBy3Or7 - {
      assert(Exercises.divBy3Or7(1, 3) == Seq(3))
      assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
      assert(
        Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21,
          24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60,
          63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99)
      )
    }
    'test_sumOfDivBy3Or5 - {
      assert(Exercises.sumOfDivBy3Or5(1, 2) == 0)
      assert(Exercises.sumOfDivBy3Or5(1, 4) == 3)
      assert(Exercises.sumOfDivBy3Or5(4, 5) == 5)
      assert(Exercises.sumOfDivBy3Or5(3, 6) == 14)
      assert(Exercises.sumOfDivBy3Or5(15, 16) == 15)
      assert(Exercises.sumOfDivBy3Or5(0, 100) == 2418)
    }
    'test_primeFactor - {
      assert(Exercises.primeFactor(0) == Seq())
      assert(Exercises.primeFactor(1) == Seq())
      assert(Exercises.primeFactor(4) == Seq(2))
      assert(Exercises.primeFactor(5) == Seq(5))
      assert(Exercises.primeFactor(80) == Seq(2, 5))
      assert(Exercises.primeFactor(98) == Seq(2, 7))
      assert(Exercises.primeFactor(2004) == Seq(2, 3, 167))
      assert(Exercises.primeFactor(-1) == Seq())
      assert(Exercises.primeFactor(-80) == Seq(2, 5))
    }
    'test_sumScalars - {
      assert(
        Exercises.sumScalars(
          Exercises.Vector2D(-1, -1),
          Exercises.Vector2D(1, 1),
          Exercises.Vector2D(-5, -5),
          Exercises.Vector2D(5, 5)
        ) == -52
      )
      assert(
        Exercises.sumScalars(
          Exercises.Vector2D(0, 0),
          Exercises.Vector2D(1, 1),
          Exercises.Vector2D(0, 5),
          Exercises.Vector2D(5, 0)
        ) == 0
      )
      assert(
        Exercises.sumScalars(
          Exercises.Vector2D(0, -1),
          Exercises.Vector2D(0, 1),
          Exercises.Vector2D(5, 0),
          Exercises.Vector2D(5, 0)
        ) == 24
      )
    }
    'test_sumCosines - {
      assert(
        Exercises
          .sumCosines(
            Exercises.Vector2D(0, 0),
            Exercises.Vector2D(1, 1),
            Exercises.Vector2D(0, 5),
            Exercises.Vector2D(5, 0)
          )
          .isNaN
      )
      assert(
        math.abs(
          Exercises.sumCosines(
            Exercises.Vector2D(1, 1),
            Exercises.Vector2D(5, 5),
            Exercises.Vector2D(5, 5),
            Exercises.Vector2D(1, 1)
          ) - 2
        ) < 0.001
      )
      assert(
        math.abs(
          Exercises.sumCosines(
            Exercises.Vector2D(1, 1),
            Exercises.Vector2D(1, 0),
            Exercises.Vector2D(0, 5),
            Exercises.Vector2D(5, 0)
          ) - math.sqrt(2) / 2
        ) < 0.001
      )
    }
  }
}
