import Exercises.{Vector2D, balls}
import utest._

object Test extends TestSuite {

  val tests = Tests {
    'test_divBy3Or7 - {
      assert(Exercises.divBy3Or7(1, 3) == Seq(3))
      assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
      assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
    }
    'test_sumOfDivBy3Or5 - {
      assert(Exercises.sumOfDivBy3Or5(0, 2) == 0)
      assert(Exercises.sumOfDivBy3Or5(1, 3) == 3)
      assert(Exercises.sumOfDivBy3Or5(1, 9) == 23)
      assert(Exercises.sumOfDivBy3Or5(0, 100) == 2418)
      assert(Exercises.sumOfDivBy3Or5(1, 1000000) == 233334166668L)
      assert(Exercises.sumOfDivBy3Or5(-1000000, 0) == -233334166668L)
    }
    'test_primeFactor - {
      assert(Exercises.primeFactor(7) == Seq(7))
      assert(Exercises.primeFactor(51) == Seq(3, 17))
      assert(Exercises.primeFactor(164) == Seq(2, 41))
      assert(Exercises.primeFactor(80) == Seq(2, 5))
      assert(Exercises.primeFactor(98) == Seq(2, 7))
      assert(Exercises.primeFactor(math.pow(2, 8).toInt) == Seq(2))
    }
    'test_sumByFunc - {
      val lv1 = Vector2D(1, 0)
      val lv2 = Vector2D(2, 2)
      val rv1 = Vector2D(1, 1)
      val rv2 = Vector2D(3, 3)
      assert(Exercises.sumScalars(lv1, lv2, rv1, rv2) == 8)
      assert(Exercises.sumCosines(lv1, lv2, rv1, rv2) - math.toRadians(45) - math.cos(0) <= 1e-10)
      val lv11 = Vector2D(12, 1)
      val lv22 = Vector2D(1, 6)
      val rv11 = Vector2D(1, 1)
      val rv22 = Vector2D(4, 10)
      assert(Exercises.sumScalars(lv11, lv22, rv11, rv22) == 32)
      assert(Exercises.sumCosines(lv11, lv22, rv11, rv22) - 0.24574667018836144 - 0.9191450300180579 <= 1e-10)
      val lv111 = Vector2D(0, 1)
      val lv222 = Vector2D(1, 0)
      val rv111 = Vector2D(2, 0)
      val rv222 = Vector2D(0, 5)
      assert(Exercises.sumScalars(lv111, lv222, rv111, rv222) == 0)
      assert(Exercises.sumCosines(lv111, lv222, rv111, rv222) - math.toRadians(2) <= 1e-10)
      val lv1111 = Vector2D(0, 0)
      val lv2222 = Vector2D(2, 2)
      val rv1111 = Vector2D(1, 1)
      val rv2222 = Vector2D(3, 3)
      assert(Exercises.sumScalars(lv1111, lv2222, rv1111, rv2222) == 6)
      assert(Exercises.sumCosines(lv1111, lv2222, rv1111, rv2222).isNaN)
    }
    'test_sortByHeavyweight - {
      assert(Exercises.sortByHeavyweight(Map()) == Seq())
      assert(Exercises.sortByHeavyweight(Map("first" -> (1, 1), "second" -> (2, 2))) == Seq("first", "second"))
      assert(Exercises.sortByHeavyweight(Map("first" -> (1, 0.4), "second" -> (4, 5.3), "third" -> (1, 1),
        "fourth" -> (2, 5), "fifth" -> (4, 7))) == Seq("first", "third", "fourth", "second", "fifth"))
    }
  }
}
