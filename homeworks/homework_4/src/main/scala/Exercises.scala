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
        for {
            i <- items.indices
            j <- items.indices
            if i != j && items(i) + items(j) == sumValue
        } return (i, j)

        (-1, -1)
    }


    def findSumFunctional(items: List[Int], sumValue: Int): (Int, Int) = {
        val idx = items.indices

        idx.iterator
          .flatMap(i =>
              idx.iterator.collect {
                  case j if i != j && items(i) + items(j) == sumValue => (i, j)
              }
          )
          .toSeq
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
    def simpleRecursion(items: List[Int], index: Int = 1): Int = items match {
        case Nil => 1
        case head :: tail =>
            val res  = simpleRecursion(tail, index + 1)
            val sign = if (head % 2 == 0) 1 else -1
            sign * head * res + index
    }

    def tailRecRecursion(items: List[Int]): Int = {
        @tailrec
        def loop(rest: List[Int], i: Int, acc: Int): Int = rest match {
            case head :: tail =>
                val newAcc =
                    if (head % 2 == 0) head * acc + i
                    else -head * acc + i

                loop(tail, i - 1, newAcc)

            case Nil => acc
        }

        loop(items.reverse, items.length, 1)
    }

    /**
     * Задание №3
     * Реализуйте алгоритм бинарного поиска, который соответсвует всем правилам функционального программирования.
     * Необходимо возвращать индекс соответствующего элемента в массиве
     * Если ответ найден, то возвращается Some(index), если нет, то None
     */

    def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
        val arr = items.toIndexedSeq

        @tailrec
        def loop(low: Int, high: Int): Option[Int] = {
            if (low > high) None
            else {
                val mid    = (low + high) / 2
                val midVal = arr(mid)

                if (midVal == value) Some(mid)
                else if (midVal < value) loop(mid + 1, high)
                else loop(low, mid - 1)
            }
        }

        loop(0, arr.length - 1)
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

        val letters = ('a' to 'z').toIndexedSeq

        def randomName(): String = {
            val len  = Random.nextInt(5) + 3 // 3..7
            val tail = (1 until len).map(_ => letters(Random.nextInt(letters.size))).mkString
            tail.capitalize
        }

        List.fill(namesCount)(randomName())
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
            if (oldPhoneRecord != null) phoneService.deletePhone(oldPhoneRecord)
            phoneService.addPhoneToBase(newPhone)
            "ok"
        }
    }

    class PhoneServiceSafety(unsafePhoneService: SimplePhoneService) {

        def findPhoneNumberSafe(num: String): Option[String] =
            Option(unsafePhoneService.findPhoneNumber(num))

        def addPhoneToBaseSafe(phone: String): Either[String, Unit] =
            try {
                unsafePhoneService.addPhoneToBase(phone)
                Right(())
            } catch {
                case _: Throwable => Left("Invalid phone number")
            }

        def deletePhoneSafe(phoneRecord: String): Either[String, Unit] =
            try {
                unsafePhoneService.deletePhone(phoneRecord)
                Right(())
            } catch {
                case _: Throwable => Left("Delete failed")
            }
    }

    class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
        override def changePhone(oldPhone: String, newPhone: String): String = {

            val deleteResult: Either[String, Unit] =
                phoneServiceSafety.findPhoneNumberSafe(oldPhone) match {
                    case Some(record) => phoneServiceSafety.deletePhoneSafe(record)
                    case None         => Right(())
                }

            deleteResult match {
                case Left(err) =>
                    err

                case Right(_) =>
                    phoneServiceSafety.addPhoneToBaseSafe(newPhone) match {
                        case Right(_)  => "ok"
                        case Left(err) => err
                    }
            }
        }
    }}}

