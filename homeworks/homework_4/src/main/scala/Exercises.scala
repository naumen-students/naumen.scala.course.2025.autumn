
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
    def findSumFunctional(items: List[Int], sumValue: Int): (Int, Int) = {
    items.indices.flatMap { i =>
        items.indices.collect { 
            case j if i != j && items(i) + items(j) == sumValue => (i, j) 
        }
    }.headOption.getOrElse((-1, -1))
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
   import scala.annotation.tailrec

def tailRecRecursion(items: List[Int]): Int = {
    @tailrec
    def loop(remaining: List[Int], index: Int, acc: Int): Int = {
        remaining match {
            case head :: tail =>
                if (head % 2 == 0) {
                    loop(tail, index + 1, head * acc + index)
                } else {
                    loop(tail, index + 1, -head * acc + index)
                }
            case Nil => acc
        }
    }
    
    loop(items, 1, 1)
}

    /**
     * Задание №3
     * Реализуйте алгоритм бинарного поиска, который соответсвует всем правилам функционального программирования.
     * Необходимо возвращать индекс соответствующего элемента в массиве
     * Если ответ найден, то возвращается Some(index), если нет, то None
     */
def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
    @annotation.tailrec
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
    
    if (items.isEmpty) None
    else search(0, items.length - 1)
}
    /**
     * Задание №4
     * Реализуйте функцию, которая генерирует список заданной длинны c именами.
     * Функция должна соответствовать всем правилам функционального программирования.
     *
     * Именем является строка, не содержащая иных символов, кроме буквенных, а также начинающаяся с заглавной буквы.
     */

  import scala.util.Random

def generateNames(namesCount: Int): List[String] = {
    if (namesCount < 0) throw new IllegalArgumentException("Invalid namesCount")
    if (namesCount == 0) return Nil
    
    val vowels = "aeiou"
    val consonants = "bcdfghjklmnpqrstvwxyz"
    
    def generateName: String = {
        val length = Random.nextInt(5) + 3 
        (0 until length).map { i =>
            if (i == 0) {
                Random.shuffle((consonants + vowels).toList).head.toUpper
            } else {
                if (i % 2 == 1) vowels(Random.nextInt(vowels.length))
                else consonants(Random.nextInt(consonants.length))
            }
        }.mkString
    }
    
    // Генерируем с запасом и берем уникальные
    LazyList.continually(generateName)
        .distinct
        .take(namesCount)
        .toList
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
            val result = unsafePhoneService.findPhoneNumber(num)
            if (result != null) Some(result) else None
        }

        def addPhoneToBaseSafe(phone: String): Either[String, Unit] = {
            try {
                unsafePhoneService.addPhoneToBase(phone)
                Right(())
            } catch {
                case e: InternalError => Left(e.getMessage)
                case e: Exception => Left(s"Unexpected error: ${e.getMessage}")
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
            
            if (!checkPhoneNumber(newPhone)) {
                return "Invalid phone string"
            }
            
            val deleteResult = phoneServiceSafety.findPhoneNumberSafe(oldPhone) match {
                case Some(phoneRecord) => 
                    phoneServiceSafety.deletePhone(phoneRecord)
                case None => 
                    Right(()) 
            }
            
            val addResult = phoneServiceSafety.addPhoneToBaseSafe(newPhone)
            
            (deleteResult, addResult) match {
                case (Right(_), Right(_)) => "ok"
                case (Left(deleteError), _) => deleteError
                case (_, Left(addError)) => addError
            }
        }
    }
}

