import utest._

object Test extends TestSuite{

  val tests = Tests{
    'test_divBy3Or7 - {
      assert(Exercises.divBy3Or7(1, 3) == Seq(3))
      assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
      assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
    }
    'test_sumOfDivBy3Or5_basic - {
      assert(Exercises.sumOfDivBy3Or5(1, 10) == 33L)
      assert(Exercises.sumOfDivBy3Or5(10, 20) == 75L)
    }
    'test_sumOfDivBy3Or5_edges - {
      assert(Exercises.sumOfDivBy3Or5(1, 1) == 0L)
      assert(Exercises.sumOfDivBy3Or5(5, 5) == 5L)
      assert(Exercises.sumOfDivBy3Or5(0, 0) == 0L)
    }

    'test_primeFactor_examples - {
      assert(Exercises.primeFactor(80) == Seq(2, 5))
      assert(Exercises.primeFactor(98) == Seq(2, 7))
      assert(Exercises.primeFactor(97) == Seq(97))
    }
    'test_primeFactor_edges - {
      assert(Exercises.primeFactor(1).isEmpty)
      assert(Exercises.primeFactor(0).isEmpty)
      assert(Exercises.primeFactor(-12) == Seq(2, 3))
    }

    'test_sumScalars - {
      val a0 = Exercises.Vector2D(1, 2)
      val a1 = Exercises.Vector2D(3, 4)
      val b0 = Exercises.Vector2D(-1, 5)
      val b1 = Exercises.Vector2D(2, 0)
      val expected = 11.0 + (-2.0)
      val actual = Exercises.sumScalars(a0, a1, b0, b1)
      assert(math.abs(actual - expected) < 1e-9)
    }
    'test_sumCosines - {
      val a0 = Exercises.Vector2D(1, 0)
      val a1 = Exercises.Vector2D(0, 1)
      val b0 = Exercises.Vector2D(2, 0)
      val b1 = Exercises.Vector2D(2, 0)
      val expected = 0.0 + 1.0
      val actual = Exercises.sumCosines(a0, a1, b0, b1)
      assert(math.abs(actual - expected) < 1e-12)
    }
    'test_sumCosines_opposite - {
      val u0 = Exercises.Vector2D(1, 0)
      val u1 = Exercises.Vector2D(-1, 0)
      val v0 = Exercises.Vector2D(0, 1)
      val v1 = Exercises.Vector2D(0, -2)
      val expected = -1.0 + -1.0
      val actual = Exercises.sumCosines(u0, u1, v0, v1)
      assert(math.abs(actual - expected) < 1e-12)
    }

    'test_sortByHeavyweight_small - {
      val tiny = Map("A" -> (1, 1.0), "B" -> (2, 0.6))
      assert(Exercises.sortByHeavyweight(tiny) == Seq("A", "B"))
    }
    'test_sortByHeavyweight_default_sorted - {
      val names = Exercises.sortByHeavyweight()
      assert(names.nonEmpty)
      assert(names.toSet == Exercises.balls.keySet)
      val masses = names.map { n =>
        val (r, d) = Exercises.balls(n)
        (4.0 / 3.0) * math.Pi * math.pow(r.toDouble, 3) * d
      }
      assert(masses.zip(masses.tail).forall { case (m1, m2) => m1 <= m2 })
    }
  }
}