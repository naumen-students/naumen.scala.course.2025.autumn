import utest._

object Test extends TestSuite {

  val tests = Tests {
    test("divBy3Or7") {
      assert(Exercises.divBy3Or7(1, 3) == Seq(3))
      assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
      assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
    }
    test("sumOfDivBy3Or5") {
      test("trivial case") {
        assert(Exercises.sumOfDivBy3Or5(3, 5) == 8)
      }
      test("not 3 or 5") {
        assert(Exercises.sumOfDivBy3Or5(1, 2) == 0)
      }
      test("multiple occurrences ") {
        assert(Exercises.sumOfDivBy3Or5(3, 10) == 33)
      }
    }
    test("primeFactor") {
      test("80 -> Seq(2, 5)") {
        assert(Exercises.primeFactor(80) == Seq(2, 5))
      }
      test("98 -> Seq(2, 7)") {
        assert(Exercises.primeFactor(98) == Seq(2, 7))
      }
      test("223_092_870") {
        assert(Exercises.primeFactor(223092870) == Seq(2, 3, 5, 7, 11, 13, 17, 19, 23))
      }
      test("362_797_056") {
        assert(Exercises.primeFactor(362797056) == Seq(2, 3))
      }
    }
    test("sortByHeavyweight") {

      assert(Exercises.sortByHeavyweight() == Seq(
        "Tin",
        "Platinum",
        "Nickel",
        "Aluminum",
        "Titanium",
        "Lead",
        "Sodium",
        "Uranium",
        "Gold",
        "Tungsten",
        "Zirconium",
        "Chrome",
        "Iron",
        "Copper",
        "Silver",
        "Plutonium",
        "Cobalt",
        "Cesium",
        "Calcium",
        "Lithium",
        "Magnesium",
        "Potassium",
        "Graphite"
      ))
    }
    test("vector") {
      val testVector: Exercises.Vector2D = Exercises.Vector2D(0, 0)
      val epsilon: Double = 1e-9
      test("abs") {
        assert(testVector.abs(Exercises.Vector2D(3, 4)) == 5.0)
        assert(testVector.abs(Exercises.Vector2D(-3, -4)) == 5.0)
        assert(testVector.abs(Exercises.Vector2D(0, 0)) == 0.0)
        assert(testVector.abs(Exercises.Vector2D(1, 1)) == math.sqrt(2))
      }
      test("scalar") {
        assert(testVector.scalar(Exercises.Vector2D(1, 2), Exercises.Vector2D(3, 4)) == 11.0)
        assert(testVector.scalar(Exercises.Vector2D(0, 0), Exercises.Vector2D(1, 1)) == 0.0)
        assert(testVector.scalar(Exercises.Vector2D(-1, -2), Exercises.Vector2D(3, 4)) == -11.0)
        assert(testVector.scalar(Exercises.Vector2D(2, 3), Exercises.Vector2D(0, 0)) == 0.0)
      }
      test("cosBetween") {
        assert(math.abs(testVector.cosBetween(Exercises.Vector2D(1, 0), Exercises.Vector2D(1, 0)) - 1.0) < epsilon )
        assert(math.abs(testVector.cosBetween(Exercises.Vector2D(1, 0), Exercises.Vector2D(0, 1)) - 0.0) < epsilon)
        assert(math.abs(testVector.cosBetween(Exercises.Vector2D(1, 0), Exercises.Vector2D(-1, 0)) + 1.0) < epsilon)
        assert(math.abs(testVector.cosBetween(Exercises.Vector2D(1, 0), Exercises.Vector2D(1, 1)) - (math.sqrt(2)/2)) < epsilon)
        assert(testVector.cosBetween(Exercises.Vector2D(0, 0), Exercises.Vector2D(1, 1)).isNaN)
      }



      test("sumByFunc") {
        test("sums results of scalar function") {
          val result = testVector.sumByFunc(
            Exercises.Vector2D(1, 2), Exercises.Vector2D(3, 4),
            testVector.scalar,
            Exercises.Vector2D(5, 6), Exercises.Vector2D(7, 8)
          )
          assert(94.0 == result) // 11 + 83
        }

        test("sums results of cosBetween function") {
          val result = testVector.sumByFunc(
            Exercises.Vector2D(1, 0), Exercises.Vector2D(1, 0),
            testVector.cosBetween,
            Exercises.Vector2D(1, 0), Exercises.Vector2D(0, 1)
          )
          assert(1.0 == result)
        }
      }

      test("sumScalars") {
        val result = testVector.sumScalars(
          Exercises.Vector2D(1, 2), Exercises.Vector2D(3, 4),
          Exercises.Vector2D(5, 6), Exercises.Vector2D(7, 8)
        )
        assert(94.0 == result)
      }

      test("sumCosines") {
        val result = testVector.sumCosines(
          Exercises.Vector2D(1, 0), Exercises.Vector2D(1, 0),
          Exercises.Vector2D(1, 0), Exercises.Vector2D(0, 1)
        )
        assert(1.0== result)
      }
    }
  }

}