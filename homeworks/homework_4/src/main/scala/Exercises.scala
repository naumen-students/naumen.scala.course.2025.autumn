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
      val indexed = items.zipWithIndex
      indexed
        .flatMap { case (a, i) =>
          indexed.map { case (b, j) =>
            if (a + b == sumValue && i != j) Some((i, j)) else None
          }.flatten
        }
        .lastOption
        .getOrElse((-1, -1))
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
      val indexed = items.zipWithIndex.map { case (v, i) => (v, i + 1) }

      @tailrec
      def loop(stack: List[(Int, Int)], acc: Int): Int = {
        stack match {
          case (value, idx) :: rest =>
            val multiplier = if (value % 2 == 0) value else -value
            loop(rest, multiplier * acc + idx)
          case Nil => acc
        }
      }
      loop(indexed.reverse, 1)
    }

    /**
     * Задание №3
     * Реализуйте алгоритм бинарного поиска, который соответсвует всем правилам функционального программирования.
     * Необходимо возвращать индекс соответствующего элемента в массиве
     * Если ответ найден, то возвращается Some(index), если нет, то None
     */

    def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
      @tailrec
      def binarySearch(low: Int, high: Int): Option[Int] = {
        if (low > high) None
        else {
          val mid = (low + high) / 2
          val midValue = items(mid)
          if (midValue == value) Some(mid)
          else if (midValue < value) binarySearch(mid + 1, high)
          else binarySearch(low, mid - 1)
        }
      }

      if (items.isEmpty) None
      else binarySearch(0, items.length - 1)
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
      if (namesCount == 0) return Nil


      @tailrec
      def gen(acc: Set[String]): List[String] = {
        if (acc.size == namesCount) acc.toList
        else {
          val newName = generateName()
          if (acc.contains(newName)) gen(acc)
          else gen(acc + newName)
        }
      }

      gen(Set.empty).toList
    }

    private def generateName(): String = {
      val vowels = "aeiou"
      val consonants = "bcdfghjklmnpqrstvwxyz"
      val len = Random.nextInt(6) + 3 // от 3 до 8
      val sb = new StringBuilder
      for (i <- 0 until len) {
        val chars = if (i % 2 == 0) consonants else vowels
        val c = chars(Random.nextInt(chars.length))
        if (i == 0) sb.append(c.toUpper) else sb.append(c)
      }
      sb.toString
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
        def findPhoneNumberSafe(num: String): Option[String] = {
          Option(unsafePhoneService.findPhoneNumber(num))
        }

        def addPhoneToBaseSafe(phone: String): Either[String, Unit] = {
          try {
            unsafePhoneService.addPhoneToBase(phone)
            Right(())
          } catch {
            case _: Throwable => Left("Failed to add phone")
          }
        }

        def deletePhone(phone: String): Either[String, Unit] = {
          try {
            unsafePhoneService.deletePhone(phone)
            Right(())
          } catch {
            case _: Throwable => Left("Failed to delete phone")
          }
        }
    }

    class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
        override def changePhone(oldPhone: String, newPhone: String): String = {
          val deletionResult = phoneServiceSafety.findPhoneNumberSafe(oldPhone) match {
            case Some(record) => phoneServiceSafety.deletePhone(record)
            case None => Right(())
          }

          deletionResult match {
            case Left(error) => error
            case Right(_) =>
              phoneServiceSafety.addPhoneToBaseSafe(newPhone) match {
                case Left(error) => error
                case Right(_) => "ok"
              }
          }
        }
    }
}