import scala.util.Random
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

    def findSumFunctional(items: List[Int], sumValue: Int) = {
        val range = 0 until items.length
        val pairs = range.flatMap(i =>
            range.zip(
                List.fill(items.length)(i)
            )
        )
        pairs.find(pair => items(pair._1) + items(pair._2) == sumValue && pair._1 != pair._2)
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

    @tailrec
    def tailRecRecursion(items: List[Int], index: Int = 1, composition: Int = 1, startAlgoritm: Boolean = true): Int = {
        val currItems = if (startAlgoritm) items.reverse else items
        val currIndex = items.length
        currItems match {
            case head :: tail =>
                if (head % 2 == 0) {
                    tailRecRecursion(tail, currIndex - 1, head * composition + currIndex, false)
                } else {
                    tailRecRecursion(tail, currIndex - 1, -1 * head * composition + currIndex, false)
                }
            case _ => composition
        }
    }

    /**
     * Задание №3
     * Реализуйте алгоритм бинарного поиска, который соответсвует всем правилам функционального программирования.
     * Необходимо возвращать индекс соответствующего элемента в массиве
     * Если ответ найден, то возвращается Some(index), если нет, то None
     */

    def functionalBinarySearch(items: List[Int], value: Int, offset: Int = 0): Option[Int] = {
        if (items.isEmpty)
            None
        else {
            val mid = items.length / 2
            val middle = items(mid)

            if (middle == value)
                Some(offset + mid)
            else if (middle > value)
                functionalBinarySearch(items.take(mid), value, offset)
            else
                functionalBinarySearch(items.drop(mid + 1), value, offset + mid + 1)
        }
    }

    /**
     * Задание №4
     * Реализуйте функцию, которая генерирует список заданной длинны c именами.
     * Функция должна соответствовать всем правилам функционального программирования.
     *
     * Именем является строка, не содержащая иных символов, кроме буквенных, а также начинающаяся с заглавной буквы.
     */

    def randomName(): String = {
        val alphabet = ('а' to 'я').toList
        val length = 2 + scala.util.Random.nextInt(10 - 2)
        val letters = List.fill(length)(alphabet(Random.nextInt(alphabet.length))) // еле нашёл
        (letters.head.toUpper :: letters.tail).mkString
    }


    def generateNames(namesСount: Int): List[String] = {
        if (namesСount < 0) {
            throw new Throwable("Invalid namesCount")
        } else {
            List.fill(namesСount)(randomName())
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
        def findPhoneNumberSafe(num: String): Option[String] = {
            val resultUnsafe = unsafePhoneService.findPhoneNumber(num)
            if (resultUnsafe == null) {
                None
            } else{
                Some(resultUnsafe)
            }
        }

        def addPhoneToBaseSafe(phone: String): Either[String, Unit] = {
            try{
                Right(unsafePhoneService.addPhoneToBase(phone))
            } catch {
                case e: InternalError => Left(e.getMessage())
            }
        }

        def deletePhone(phone: String) : Unit = unsafePhoneService.deletePhone(phone)
    }

    class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
        override def changePhone(oldPhone: String, newPhone: String): String = {
            val findedOldPhone = phoneServiceSafety.findPhoneNumberSafe(oldPhone)
            findedOldPhone match {
                case None => "not found old phone"
                case Some(value) => {
                    phoneServiceSafety.deletePhone(value)
                    val resultAppendPhone = phoneServiceSafety.addPhoneToBaseSafe(newPhone)
                    resultAppendPhone match {
                        case Left(errorMessage) => {
                            errorMessage
                        }
                        case Right(value) => {
                            "ok"
                        }
                    }
                }
            }

        }
    }
}
