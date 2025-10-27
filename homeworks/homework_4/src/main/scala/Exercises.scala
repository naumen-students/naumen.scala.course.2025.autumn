import scala.util.{Failure, Success, Try}

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
        items.zipWithIndex.reverse
          .flatMap { case (item1, i) =>
              items.zipWithIndex.reverse
                .collect { case (item2, j) if i != j && item1 + item2 == sumValue => (i, j) }
          }
          .headOption
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

    import scala.annotation.tailrec

    @tailrec
    def tailRecRecursion(items: List[Int], index: Int = 1, sum: Int = 0, m: Int = 1): Int = {
        items match {
            case head :: tail =>
                if (head % 2 == 0) {
                    tailRecRecursion(tail, index + 1, sum + m * index, m * head)
                } else {
                    tailRecRecursion(tail, index + 1, sum + m * index, -m * head)
                }
            case _ => sum + m
        }
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
                    case x if x == value => Some(mid)
                    case x if x < value => search(mid + 1, high)
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
        import scala.util.Random

        val minNameLength = 3
        val maxNameLength = 12
        val firstLetterStart = 65 // char code of symbol A
        val firstLetterEnd = 90 // Z
        val otherLettersStart = 97 // a
        val otherLettersEnd = 122 // z

        def generateName: String = {
            val nameLength = Random.nextInt(maxNameLength - minNameLength + 1) + minNameLength
            val firstChar = (Random.nextInt(firstLetterEnd - firstLetterStart + 1) + firstLetterStart).toChar
            val otherChars = (1 until nameLength).map { _ =>
                (Random.nextInt(otherLettersEnd - otherLettersStart + 1) + otherLettersStart).toChar
            }.mkString
            firstChar + otherChars
        }

        return Iterator
          .continually(generateName)
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
 *0
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


    private class PhoneServiceSafety(unsafePhoneService: SimplePhoneService) {
        def findPhoneNumberSafe(num: String): Option[String] = {
            val phoneNumber = unsafePhoneService.findPhoneNumber(num)
            phoneNumber match {
                case null => None
                case _ => Some(phoneNumber)
            }
        }

        def addPhoneToBaseSafe(phone: String): Try[Unit] = {
            Try(unsafePhoneService.addPhoneToBase(phone))
        }

        def deletePhone(phone: String): Unit = {
            unsafePhoneService.deletePhone(phone)
        }
    }

    class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
        override def changePhone(oldPhone: String, newPhone: String): String = {
            phoneServiceSafety.findPhoneNumberSafe(oldPhone).foreach(phoneServiceSafety.deletePhone)
            phoneServiceSafety.addPhoneToBaseSafe(newPhone) match {
                case Failure(_) => "failure"
                case Success(_) => "ok"
            }
        }
    }
}