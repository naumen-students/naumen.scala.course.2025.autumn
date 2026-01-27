import scala.annotation.tailrec

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
            indexed.map { case (b, j) => (a, b, i, j)}
        }
        .filter { case (a, b, i, j) =>
            i != j && a + b == sumValue
        }
        .map { case (_, _, i, j) => (i, j) }
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
        @tailrec
        def buildReverseList(items: List[Int], index: Int, acc: List[(Int, Int)]): List[(Int, Int)] = {
            items match {
            case head :: tail =>
                buildReverseList(tail, index + 1, (head, index) :: acc)
            case Nil =>
                acc
            }
        }

        @tailrec
        def tailRecursion(items: List[(Int, Int)], acc: Int = 1): Int = {
            items match {
                case (value, index) :: tail =>
                    if (value % 2 == 0) {
                        tailRecursion(tail, acc * value + index)
                    } else {
                        tailRecursion(tail, acc * value * (-1) + index)
                    }
                case _ => acc
            }
        }  
        val reverseList = buildReverseList(items, 1, Nil)
        tailRecursion(reverseList, 1) 
    }

    /**
     * Задание №3
     * Реализуйте алгоритм бинарного поиска, который соответсвует всем правилам функционального программирования.
     * Необходимо возвращать индекс соответствующего элемента в массиве
     * Если ответ найден, то возвращается Some(index), если нет, то None
     */

    def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
        @tailrec
        def recursive(items: List[Int], left: Int, right: Int, value: Int) : Option[Int] = {
            val mid = (right + left) / 2
            if (left > right) {
                None
            } else if (items(mid) == value) {
                Some(mid)
            } else if (items(mid) < value) {
                recursive(items, mid + 1, right, value)
            } else {
                recursive(items, left, mid - 1, value)
            }
        }
        if (items.isEmpty) None else recursive(items, 0, items.length - 1, value)
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
        else {
            val upper = ('A' to 'Z').toList ++ ('А' to 'Я').toList
            val lower = ('a' to 'z').toList ++ ('а' to 'я').toList

            val allNames: Stream[String] = for {
            a <- upper.toStream
            b <- lower.toStream
            c  <- lower.toStream
            } yield s"$a$b$c"

            allNames.take(namesСount).toList
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
        def findPhoneNumberSafe(num: String) : Either[String, String] = {
            val findPhoneNumber = unsafePhoneService.findPhoneNumber(num);
            if (findPhoneNumber == null) {
                Left(s"can't find the phone number '$num'")
            } else {
                Right(findPhoneNumber)
            }
        }

        def addPhoneToBaseSafe(phone: String) : Either[String, String] = {
            try {
                unsafePhoneService.addPhoneToBase(phone)
                Right("ok")
            }
            catch {
                case e: InternalError => Left(e.getMessage)
            }
        }

        def deletePhone(phone: String): Unit = {
            unsafePhoneService.deletePhone(phone)
        }
    }

    class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
        override def changePhone(oldPhone: String, newPhone: String): String = {
            val oldPhoneRecord = phoneServiceSafety.findPhoneNumberSafe(oldPhone)
            oldPhoneRecord match {
                case Right(findPhoneNumber) => phoneServiceSafety.deletePhone(findPhoneNumber)
                case Left(text) => text
            }

            phoneServiceSafety.addPhoneToBaseSafe(newPhone) match {
                case Right(ok) => ok
                case Left(error) => error
            }
        }
    }
}
