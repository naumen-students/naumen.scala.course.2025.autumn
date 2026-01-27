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

  def findSumFunctional(items: List[Int], sumValue: Int) = {
    val allPairs = for {
      i <- items.indices
      j <- items.indices
      if i != j && items(i) + items(j) == sumValue
    } yield (i, j)

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

  def tailRecRecursion(items: List[Int]): Int = {

    def loop(remaining: List[Int], index: Int, acc: Int): Int = {
      remaining match {
        case Nil => acc
        case head :: tail =>
          val recursiveResult = if (tail.nonEmpty) loop(tail, index + 1, acc) else 1
          if (head % 2 == 0) {
            head * recursiveResult + index
          } else {
            -head * recursiveResult + index
          }
      }
    }

    if (items.isEmpty) 1 else loop(items, 1, 1)
  }

  /**
   * Задание №3
   * Реализуйте алгоритм бинарного поиска, который соответсвует всем правилам функционального программирования.
   * Необходимо возвращать индекс соответствующего элемента в массиве
   * Если ответ найден, то возвращается Some(index), если нет, то None
   */

  def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
    @tailrec
    def search(low: Int, high: Int): Option[Int] = {
      if (low > high) None
      else {
        val mid = low + (high - low) / 2
        items(mid) match {
          case mv if mv == value => Some(mid)
          case mv if mv < value => search(mid + 1, high)
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
    def generateName(seed: Int): String = {
      val length = (seed % 6) + 3

      val firstChar = ('A' + (seed % 26)).toChar
      val restChars = (1 until length).map { i =>
        ('a' + ((seed * i) % 26)).toChar
      }.mkString
      firstChar + restChars
    }
    Stream.from(0)
      .map(generateName)
      .distinct
      .take(namesCount)
      .toList
  }

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
  import Utils._

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
    def findPhoneNumberSafe(num: String) = {
      val result = unsafePhoneService.findPhoneNumber(num)
      Option(result)
    }

    def addPhoneToBaseSafe(phone: String) = {
      try {
        unsafePhoneService.addPhoneToBase(phone)
        Right(())
      } catch {
        case e: Exception => Left(s"Failed to add phone: ${e.getMessage}")
      }
    }

    def deletePhone(phone: String) = {
      try {
        unsafePhoneService.deletePhone(phone)
        Right(())
      } catch {
        case e: Exception => Left(s"Failed to delete phone: ${e.getMessage}")
      }
    }
  }

  class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
    override def changePhone(oldPhone: String, newPhone: String): String = {
      phoneServiceSafety.findPhoneNumberSafe(oldPhone) match {
        case Some(existingPhone) =>
          // Используем явное связывание Either-ов
          phoneServiceSafety.deletePhone(existingPhone) match {
            case Left(error) => error
            case Right(_) =>
              phoneServiceSafety.addPhoneToBaseSafe(newPhone) match {
                case Left(error) => error
                case Right(_) => "ok"
              }
          }
        case None =>
          phoneServiceSafety.addPhoneToBaseSafe(newPhone) match {
            case Left(error) => error
            case Right(_) => "ok"
          }
      }
    }
  }
}