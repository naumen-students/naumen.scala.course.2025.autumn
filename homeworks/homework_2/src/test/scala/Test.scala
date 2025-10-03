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
      assert(Exercises.sumOfDivBy3Or5(1, 1) == 0)
      assert(Exercises.sumOfDivBy3Or5(0, 0) == 0)
      assert(Exercises.sumOfDivBy3Or5(10, 15) == (10 + 12 + 15))
    }

    'test_primeFactor - {
      assert(Exercises.primeFactor(80) == Seq(2, 5))
      assert(Exercises.primeFactor(98) == Seq(2, 7))
      assert(Exercises.primeFactor(13) == Seq(13))
      assert(Exercises.primeFactor(2) == Seq(2))
    }

    'test_sumScalars - {
      val v1 = Exercises.Vector2D(1, 0)
      val v2 = Exercises.Vector2D(0, 1)
      val v3 = Exercises.Vector2D(1, 1)
      val v4 = Exercises.Vector2D(2, 2)
      val expected = Exercises.scalar(v1, v2) + Exercises.scalar(v3, v4)
      assert(Exercises.sumScalars(v1, v2, v3, v4) == expected)
    }

    'test_sumCosines - {
      val v1 = Exercises.Vector2D(1, 0)
      val v2 = Exercises.Vector2D(0, 1)
      val v3 = Exercises.Vector2D(1, 1)
      val v4 = Exercises.Vector2D(2, 2)
      val expected = Exercises.cosBetween(v1, v2) + Exercises.cosBetween(v3, v4)
      assert(math.abs(Exercises.sumCosines(v1, v2, v3, v4) - expected) < 1e-9)
    }

    'test_sortByHeavyweight - {
      val sorted = Exercises.sortByHeavyweight()
      val massMap = Exercises.balls.map { case (name, (r, d)) =>
        name -> (4.0 / 3.0) * math.Pi * math.pow(r, 3) * d
      }

      val lightest = massMap.minBy(_._2)._1
      assert(sorted.head == lightest)

      val heaviest = massMap.maxBy(_._2)._1
      assert(sorted.last == heaviest)

      assert(sorted.toSet == Exercises.balls.keySet)
    }
  }
}
