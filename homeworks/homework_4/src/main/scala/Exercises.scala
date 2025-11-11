import scala.annotation.tailrec
import scala.util.Random

object Exercises {

    /**
     * Задание №1
     * Дана императивная функция findSumImperative.
     * Напишите ее аналог (findSumFunctional) в функциональном стиле.
     *
     * ПОДСКАЗКА
     * Стоит воспользоваться методами, которые предоставляет объект List или рекурсией.
     * Страница с полезностями List: https://alvinalexander.com/scala/list-class-methods-examples-syntax/
     */
    def findSumImperative(items: List[Int], sumValue: Int): (Int, Int) = {
        var result: (Int, Int) = (-1, -1)
        for (i <- 0 until items.length) {
            for (j <- 0 until items.length) {
                if (items(i) + items(j) == sumValue && i != j) {
                    result = (i, j)
                }
            }
        }
        result
    }

  def findSumFunctional(items: List[Int], sumValue: Int): (Int, Int) = {
    val indexedItems = items.zipWithIndex

    // Генерируем все возможные пары и берем последнюю подходящую
    val allPairs = for {
      (num1, idx1) <- indexedItems
      (num2, idx2) <- indexedItems
      if idx1 != idx2 && num1 + num2 == sumValue
    } yield (idx1, idx2)

    allPairs.lastOption.getOrElse((-1, -1))
  }


    /**
     * Задание №2
     *
     * Дана рекурсивная функция simpleRecursion.
     * Перепишите ее так, чтобы получилась хвостовая рекурсивная функция.
     *
     * Для прохождения теста на большое количество элементов в списке
     * используйте анотацию @tailrec к вашей функции.
     */
    def simpleRecursion(items: List[Int], index: Int = 1): Int = {
        items match {
            case head :: tail =>
                if (head % 2 == 0) {
                    head * simpleRecursion(tail, index + 1) + index
                } else {
                    -1 * head * simpleRecursion(tail, index + 1) + index
                }
            case _ => 1
        }
    }

  @tailrec
  final def tailRecRecursionHelper(items: List[Int], index: Int, accMultiplier: Int, accSum: Int): Int = {
    items match {
      case head :: tail =>
        if (head % 2 == 0) {
          tailRecRecursionHelper(tail, index + 1, accMultiplier * head, accSum + index * accMultiplier)
        } else {
          tailRecRecursionHelper(tail, index + 1, accMultiplier * (-head), accSum + index * accMultiplier)
        }
      case _ => accSum + accMultiplier * 1
    }
  }

  def tailRecRecursion(items: List[Int]): Int = tailRecRecursionHelper(items, 1, 1, 0)

    /**
     * Задание №3
     * Реализуйте алгоритм бинарного поиска, который соответсвует всем правилам функционального программирования.
     * Необходимо возвращать индекс соответствующего элемента в массиве
     * Если ответ найден, то возвращается Some(index), если нет, то None
     */

    def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
      @tailrec
      def search(low: Int, high: Int): Option[Int] = {
        if (low > high) {
          None
        } else {
          val mid = low + (high - low) / 2
          items(mid) match {
            case midValue if midValue == value => Some(mid)
            case midValue if midValue < value => search(mid + 1, high)
            case _ => search(low, mid - 1)
          }
        }
      }

      search(0, items.length - 1)
    }

    /**
     * Задание №4
     * Реализуйте функцию, которая генерирует список заданной длинны c именами.
     * Функция должна соответствовать всем правилам функционального программирования.
     *
     * Именем является строка, не содержащая иных символов, кроме буквенных, а также начинающаяся с заглавной буквы.
     */

    def generateNames(namesCount: Int): List[String] = {
      if (namesCount < 0) throw new IllegalArgumentException("Invalid namesCount")
      if (namesCount == 0) return List.empty[String]

      val random = new Random()

      // Только латинские буквы (согласно тесту)
      val uppercase = ('A' to 'Z').toVector
      val lowercase = ('a' to 'z').toVector

      // Используем Set для гарантии уникальности
      var generatedNames = Set[String]()

      def generateName(): String = {
        // Длина от 2 до 20 символов (достаточно для уникальности)
        val length = 2 + random.nextInt(19)

        // Первая буква - заглавная
        val firstChar = uppercase(random.nextInt(uppercase.length))

        // Остальные буквы - строчные
        val restChars = (1 until length).map { _ =>
          lowercase(random.nextInt(lowercase.length))
        }.mkString

        firstChar + restChars
      }

      // Генерируем уникальные имена
      while (generatedNames.size < namesCount) {
        val name = generateName()
        if (!generatedNames.contains(name)) {
          generatedNames += name
        }
      }

      generatedNames.toList
    }

/**
 * Задание №5
 *
 * Дана реализация сервиса по смене номера SimpleChangePhoneService с методом changePhone
 * Необходимо написать реализацию этого сервиса с учетом правил работы со сторонними эффектами (SideEffects).
 *
 * Для этого необходимо сначала реализовать собственный сервис работы с телефонными номерами (PhoneServiceSafety),
 * используя при этом методы из unsafePhoneService.
 * Методы должны быть безопасными, поэтому тип возвращаемых значений необходимо определить самостоятельно.
 * Рекомендуется воспользоваться стандартными типами Scala (например Option или Either).
 *
 * Затем, с использованием нового сервиса, необходимо реализовать "безопасную" версию функции changePhone.
 * Функция должна возвращать ok в случае успешного завершения или текст ошибки.
 *
 * Изменять методы внутри SimplePhoneService не разрешается.
 */

object SideEffectExercise {
  // import Utils._ // Раскомментируйте если есть файл Utils

  // Базовые интерфейсы и классы для задания 5
  trait ChangePhoneService {
    def changePhone(oldPhone: String, newPhone: String): String
  }

  class SimplePhoneService {
    def findPhoneNumber(phone: String): String = {
      // Реализация поиска телефона
      if (phone == "123") "123" else null
    }

    def deletePhone(phone: String): Unit = {
      // Реализация удаления телефона
      if (phone == "invalid") throw new Exception("Cannot delete invalid phone")
    }

    def addPhoneToBase(phone: String): Unit = {
      // Реализация добавления телефона
      if (phone == "invalid") throw new Exception("Cannot add invalid phone")
    }
  }

  class SimpleChangePhoneService(phoneService: SimplePhoneService) extends ChangePhoneService {
    override def changePhone(oldPhone: String, newPhone: String): String = {
      val oldPhoneRecord = phoneService.findPhoneNumber(oldPhone)
      if (oldPhoneRecord != null) {
        phoneService.deletePhone(oldPhoneRecord)
      }
      phoneService.addPhoneToBase(newPhone)
      "ok"
    }
  }

  class PhoneServiceSafety(unsafePhoneService: SimplePhoneService) {
    def findPhoneNumberSafe(num: String): Option[String] = {
      Option(unsafePhoneService.findPhoneNumber(num))
    }

    def addPhoneToBaseSafe(phone: String): Either[String, Unit] = {
      try {
        unsafePhoneService.addPhoneToBase(phone)
        Right(())
      } catch {
        case e: Exception => Left(e.getMessage)
      }
    }

    def deletePhone(phone: String): Either[String, Unit] = {
      try {
        unsafePhoneService.deletePhone(phone)
        Right(())
      } catch {
        case e: Exception => Left(e.getMessage)
      }
    }
  }

  class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
    override def changePhone(oldPhone: String, newPhone: String): String = {
      // Обрабатываем старый телефон
      val oldPhoneResult = phoneServiceSafety.findPhoneNumberSafe(oldPhone)

      // Удаляем старый телефон, если он существует
      val deleteResult = oldPhoneResult match {
        case Some(phone) => phoneServiceSafety.deletePhone(phone)
        case None => Right(()) // Если телефона нет, ничего не удаляем
      }

      // Добавляем новый телефон
      val addResult = deleteResult.flatMap(_ =>
        phoneServiceSafety.addPhoneToBaseSafe(newPhone)
      )

      // Возвращаем результат
      addResult match {
        case Right(_) => "ok"
        case Left(error) => error
      }
    }
  }
}

} // Закрывающая скобка для объекта Exercises