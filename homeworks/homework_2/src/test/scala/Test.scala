import utest._

object Test extends TestSuite {

  val tests = Tests {
    'test_divBy3Or7 - {
      assert(Exercises.divBy3Or7(1, 3) == Seq(3))
      assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
      }

    'test_sumOfDivBy3Or5 - {
      // Тест 1: диапазон 1-10 (3+5+6+9+10 = 33)
      assert(Exercises.sumOfDivBy3Or5(1, 10) == 33L)

      // Тест 2: диапазон без подходящих чисел
      assert(Exercises.sumOfDivBy3Or5(1, 2) == 0L)

      // Тест 3: граничные случаи
      assert(Exercises.sumOfDivBy3Or5(0, 0) == 0L) // 0 делится на любое число, но в задании может быть особый случай
      assert(Exercises.sumOfDivBy3Or5(3, 5) == 8L) // 3 + 5 = 8
    }

    'test_primeFactor - {
      // Тест для числа 80
      assert(Exercises.primeFactor(80) == Seq(2, 5))

      // Тест для числа 98
      assert(Exercises.primeFactor(98) == Seq(2, 7))

      // Тест для простого числа
      assert(Exercises.primeFactor(17) == Seq(17))

      // Тест для чисел меньше 2
      assert(Exercises.primeFactor(1) == Seq())
      assert(Exercises.primeFactor(0) == Seq())

      // Тест для числа 36 (2^2 * 3^2)
      assert(Exercises.primeFactor(36) == Seq(2, 3))
    }

    'test_vectorOperations - {
      val vec0 = Exercises.Vector2D(0, 0)
      val vec1 = Exercises.Vector2D(1, 0)
      val vec2 = Exercises.Vector2D(0, 1)
      val vec3 = Exercises.Vector2D(1, 1)
      val vec4 = Exercises.Vector2D(2, 2)

      'test_sumScalars - {
        // Перпендикулярные векторы: скалярное произведение = 0
        assert(Exercises.sumScalars(vec1, vec2, vec1, vec2) == 0.0)

        // Параллельные векторы: vec3(1,1) и vec4(2,2)
        // scalar(vec3, vec4) = 1*2 + 1*2 = 2 + 2 = 4
        // sumScalars = 4 + 4 = 8
        assert(Exercises.sumScalars(vec3, vec4, vec3, vec4) == 8.0)

        // Нулевые векторы
        assert(Exercises.sumScalars(vec0, vec1, vec0, vec1) == 0.0)

        // Дополнительные проверки для уверенности
        assert(Exercises.scalar(vec3, vec4) == 4.0)
        assert(Exercises.scalar(vec1, vec1) == 1.0)
      }

      'test_sumCosines - {
        // Одинаковые векторы: косинус угла = 1
        assert(math.abs(Exercises.sumCosines(vec1, vec1, vec2, vec2) - 2.0) < 0.0001)

        // Перпендикулярные векторы: косинус = 0
        assert(math.abs(Exercises.sumCosines(vec1, vec2, vec1, vec2)) < 0.0001)

        // Параллельные векторы: косинус = 1
        // vec3 и vec4 параллельны, cosBetween = 1
        assert(math.abs(Exercises.sumCosines(vec3, vec4, vec3, vec4) - 2.0) < 0.0001)

        // Проверка косинуса между параллельными векторами
        assert(math.abs(Exercises.cosBetween(vec3, vec4) - 1.0) < 0.0001)
      }
    }

    'test_sortByHeavyweight - {
      val testBalls = Map(
        "SmallLight" -> (1, 1.0),
        "LargeHeavy" -> (2, 2.0)
      )
      assert(Exercises.sortByHeavyweight(testBalls) == Seq("SmallLight", "LargeHeavy"))

      val threeBalls = Map(
        "A" -> (1, 10.0),  // масса ≈ 41.9 г
        "B" -> (2, 1.0),   // масса ≈ 33.51 г
        "C" -> (3, 0.1)    // масса ≈ 11.31 г
      )
      // Правильный порядок: C (легкий), B, A (тяжелый)
      assert(Exercises.sortByHeavyweight(threeBalls) == Seq("C", "B", "A"))

      assert(Exercises.sortByHeavyweight().nonEmpty)
    }
  }
}
