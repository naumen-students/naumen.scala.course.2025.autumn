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
    val pairs = for {
      i <- items.indices
      j <- items.indices
      if i != j && items(i) + items(j) == sumValue
    } yield (i, j)

    pairs.lastOption.getOrElse((-1, -1))
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
    def tailRecRecursion(items: List[Int], acc: Int = 1, index: Int = 0): Int = {
      if (items.isEmpty) {
        acc
      }
      else if (index == 0) {
        tailRecRecursion(items.reverse, acc, items.length)
      } else {
        items match {
          case head :: tail =>
            val factor =  if (head % 2 == 0) head else -head
            val nextAcc = factor * acc + index
            tailRecRecursion(tail, nextAcc, index - 1)
        }
      }
    }

    /**
     * Задание №3
     * Реализуйте алгоритм бинарного поиска, который соответсвует всем правилам функционального программирования.
     * Необходимо возвращать индекс соответствующего элемента в массиве
     * Если ответ найден, то возвращается Some(index), если нет, то None
     */

    @tailrec
    def functionalBinarySearch(items: List[Int], value: Int, indexDiff: Int = 0): Option[Int] = {
      val index = items.length / 2

      if (items.isEmpty) {
        None
      } else if (items(index) == value) {
        Some(index + indexDiff)
      } else if (items(index) > value) {
        functionalBinarySearch(items.take(index), value, indexDiff)
      } else {
        functionalBinarySearch(items.drop(index + 1), value, indexDiff + index + 1)
      }
    }

    /**
     * Задание №4
     * Реализуйте функцию, которая генерирует список заданной длинны c именами.
     * Функция должна соответствовать всем правилам функционального программирования.
     *
     * Именем является строка, не содержащая иных символов, кроме буквенных, а также начинающаяся с заглавной буквы.
     */

    def generateNames(namesCount: Int): List[String] = {
      if (namesCount < 0) throw new Throwable("Invalid namesCount")

      (1 to namesCount).toList.map{ _ =>
        val nameLen = Random.nextInt(10) + 2
        val firstChar = (Random.nextInt(26) + 'A').toChar
        val otherChars = (1 until nameLen).map{ _ =>
          (Random.nextInt(26) + 'a').toChar
        }.mkString
        firstChar + otherChars
      }
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
      def findPhoneNumberSafe(num: String): Either[String, String] = {
        val result = unsafePhoneService.findPhoneNumber(num)
        if (result == null) {
          Left("not found")
        } else {
          Right(result)
        }
      }

        def addPhoneToBaseSafe(phone: String): Either[String, String] = {
          try {
            unsafePhoneService.addPhoneToBase(phone)
            Right("ok")
          } catch {
            case e: InternalError => Left(e.getMessage)
          }
        }

        def deletePhone(phone: String): Either[String, String] = {
          unsafePhoneService.deletePhone(phone)
          Right("ok")
        }
    }

    class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
        override def changePhone(oldPhone: String, newPhone: String): String = {
          val oldPhoneResult = phoneServiceSafety.findPhoneNumberSafe(oldPhone)

          oldPhoneResult match {
            case Right(phone) => phoneServiceSafety.deletePhone(phone)
            case Left(_) => ()
          }

          val addResult = phoneServiceSafety.addPhoneToBaseSafe(newPhone)
          addResult match {
            case Right(_) => "ok"
            case Left(err) => err
          }
        }
    }
}
