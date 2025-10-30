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
        items.indices
          .flatMap(i => items.indices.map(j => (i, j)))
          .filter { case (i, j) => i != j && items(i) + items(j) == sumValue }
          .lastOption
          .getOrElse(-1, -1)
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
        @tailrec
        def innerRec(l: List[Int], ind: Int, acc: Int): Int = {
            l match {
                case head :: tail =>
                    val newAcc =
                        if (head % 2 == 0) {
                            head * acc + ind
                        } else {
                            - head * acc + ind
                        }
                    innerRec(tail, ind - 1, newAcc)
                case Nil => acc
            }
        }
        innerRec(items.reverse, items.length, 1)
    }

    /**
     * Задание №3
     * Реализуйте алгоритм бинарного поиска, который соответсвует всем правилам функционального программирования.
     * Необходимо возвращать индекс соответствующего элемента в массиве
     * Если ответ найден, то возвращается Some(index), если нет, то None
     */

    def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
        @tailrec
        def innerBinarySearch(l: Int, r: Int): Option[Int] = {
            if (l > r) {
                None
            } else {
                val m = l + (r - l) / 2
                val mVal = items(m)
                if (mVal == value) {
                    Some(m)
                } else if (mVal < value) {
                    innerBinarySearch(m + 1, r)
                } else {
                    innerBinarySearch(l, m - 1)
                }
            }
        }
        if (items.isEmpty) {
            None
        } else {
            innerBinarySearch(0, items.length - 1)
        }
    }

    /**
     * Задание №4
     * Реализуйте функцию, которая генерирует список заданной длинны c именами.
     * Функция должна соответствовать всем правилам функционального программирования.
     *
     * Именем является строка, не содержащая иных символов, кроме буквенных, а также начинающаяся с заглавной буквы.
     */

    def generateNames(namesСount: Int): List[String] = {
        if (namesСount < 0) throw new Throwable("Invalid namesCount")
        @tailrec
        def innerGenerator(c: Int, rnd: Random, acc: List[String]): List[String] = {
            if (c <= 0) acc
            else {
                val len = rnd.nextInt(42) + 7
                val first = ('A' + rnd.nextInt(26)).toChar
                val end = (1 until len).map(_ => ('a' + rnd.nextInt(26)).toChar).mkString
                val name = s"$first$end"
                innerGenerator(c - 1, rnd, name :: acc)
            }
        }
        innerGenerator(namesСount, new Random(), Nil)
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
            val found = unsafePhoneService.findPhoneNumber(num)
            if (found == null) {
                None
            } else {
                Some(found)
            }
        }

        def addPhoneToBaseSafe(phone: String): Either[String, Unit] = {
            try {
                unsafePhoneService.addPhoneToBase(phone)
                Right(())
            } catch {
                case e: Throwable => Left(e.getMessage)
            }
        }

        def deletePhone(phone: String): Either[String, Unit] = {
            try {
                unsafePhoneService.deletePhone(phone)
                Right(())
            } catch {
                case e: Throwable => Left(e.getMessage)
            }
        }
    }

    class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
        override def changePhone(oldPhone: String, newPhone: String): String = {
            val delOldPhone = phoneServiceSafety.findPhoneNumberSafe(oldPhone) match {
                case Some(_) => phoneServiceSafety.deletePhone(oldPhone)
                case None => Right(())
            }

            delOldPhone match {
                case Left(e) => e
                case Right(_) =>
                    phoneServiceSafety.addPhoneToBaseSafe(newPhone) match {
                        case Right(_) => "ok"
                        case Left(e) => e
                    }
            }
        }
    }
}
