import utest._

object Test extends TestSuite {

  val tests = Tests {
    'test_divBy3Or7 - {
      assert(Exercises.divBy3Or7(1, 3) == Seq(3))
      assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
      assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
    }
    'sumOfDivBy3Or5 - {
      assert(Exercises.sumOfDivBy3Or5(1, 10) == 33)
      assert(Exercises.sumOfDivBy3Or5(1, 5) == 8)
      assert(Exercises.sumOfDivBy3Or5(1, 2) == 0)
      assert(Exercises.sumOfDivBy3Or5(11, 11) == 0)
      assert(Exercises.sumOfDivBy3Or5(-10, 0) == -33)
      assert(Exercises.sumOfDivBy3Or5(10, 1) == 0)
    }

    'primeFactor - {
      assert(Exercises.primeFactor(80).sorted == Seq(2, 5))
      assert(Exercises.primeFactor(98).sorted == Seq(2, 7))
      assert(Exercises.primeFactor(12).sorted == Seq(2, 3))
      assert(Exercises.primeFactor(30).sorted == Seq(2, 3, 5))

      assert(Exercises.primeFactor(7) == Seq(7))
      assert(Exercises.primeFactor(13) == Seq(13))
      assert(Exercises.primeFactor(2) == Seq(2))

      assert(Exercises.primeFactor(1) == Seq.empty)
      assert(Exercises.primeFactor(0) == Seq.empty)
      assert(Exercises.primeFactor(-5) == Seq.empty)
    }

    'vector_operations - {
      val leftVec0 = Exercises.Vector2D(1, 2)
      val leftVec1 = Exercises.Vector2D(3, 4)
      val rightVec0 = Exercises.Vector2D(5, 6)
      val rightVec1 = Exercises.Vector2D(7, 8)
      val vec0 = Exercises.Vector2D(1, 0)
      val vec1 = Exercises.Vector2D(0, 1)
      val oppositeVec = Exercises.Vector2D(-1, 0)

      'sumScalars - {
        assert(Exercises.sumScalars(leftVec0, leftVec1, rightVec0, rightVec1) == 94.0)
        assert(Exercises.sumScalars(vec0, vec1, vec0, vec1) == 0.0)
      }

      'sumCosines - {
        assert(Exercises.sumCosines(vec0, vec0, vec0, vec1) == 1.0)
        assert(Exercises.sumCosines(vec0, vec0, vec0, vec0) == 2.0)
        assert(Exercises.sumCosines(vec0, vec1, vec0, vec1) == 0.0)
        assert(Exercises.sumCosines(vec0, oppositeVec, vec0, oppositeVec) == -2.0)
      }
    }


    'sortByHeavyweight - {
      'getWeight - {
        val density = 3.0
        val radius = 2
        val expectedVolume = (4.0 / 3.0) * math.Pi * 8.0
        val expectedWeight = expectedVolume * density

        assert(Exercises.getWeight(radius, density) == expectedWeight)

        assert(Exercises.getWeight(0, 5.0) == 0.0)
        assert(Exercises.getWeight(5, 0.0) == 0.0)
      }
      
      'full - {
        val result = Exercises.sortByHeavyweight()
        assert(result.head == "Tin")
        assert(result.last == "Graphite")
        assert(result.size == 23)
        assert(result.toSet == Exercises.balls.keySet)
      }

      'empty - {
        assert(Exercises.sortByHeavyweight(Map.empty[String, (Int, Double)]) == Seq.empty)
      }

      'single - {
        val balls = Map("ball" -> (1, 1.0))
        assert(Exercises.sortByHeavyweight(balls) == Seq("ball"))
      }

      'more_than_1 - {
        val balls = Map(
          "LargeLight" -> (5, 0.1),
          "SmallHeavy" -> (1, 10.0)
        )
        val result = Exercises.sortByHeavyweight(balls)
        assert(result.head == "SmallHeavy")
        assert(result.last == "LargeLight")
      }
    }
  }
}
