import scala.annotation.tailrec
import scala.util.Random

object Exercises {

  /** Задание №1
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
    val sumValues = for {
      i <- items.indices
      j <- items.indices
      if items(i) + items(j) == sumValue && i != j
    } yield (i, j)

    if (sumValues.isEmpty) {
      (-1, -1)
    } else {
      sumValues.last
    }
  }

  /** Задание №2
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
    def innerTailRecRecursion(
        remaining: List[Int],
        index: Int,
        accumulation: Int
    ): Int = {
      remaining match {
        case init :+ last =>
          val newAcc =
            if (last % 2 == 0)
              last * accumulation + index
            else
              -1 * last * accumulation + index
          innerTailRecRecursion(init, index - 1, newAcc)
        case _ => accumulation
      }
    }

    innerTailRecRecursion(items, items.length, 1)
  }

  /** Задание №3
    * Реализуйте алгоритм бинарного поиска, который соответсвует всем правилам функционального программирования.
    * Необходимо возвращать индекс соответствующего элемента в массиве
    * Если ответ найден, то возвращается Some(index), если нет, то None
    */
  def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
    @tailrec
    def binarySearch(left: Int, right: Int): Option[Int] = {
      if (left > right)
        return None
      val middle = left + (right - left) / 2
      items(middle) compare value match {
        case 0          => Some(middle)
        case x if x < 0 => binarySearch(middle + 1, right)
        case _          => binarySearch(left, middle - 1)
      }

    }

    if (items.isEmpty) None else binarySearch(0, items.length - 1)
  }

  /** Задание №4
    * Реализуйте функцию, которая генерирует список заданной длинны c именами.
    * Функция должна соответствовать всем правилам функционального программирования.
    *
    * Именем является строка, не содержащая иных символов, кроме буквенных, а также начинающаяся с заглавной буквы.
    */
  def generateNames(namesCount: Int): List[String] = {
    if (namesCount < 0) throw new Throwable("Invalid namesCount")

    val alphabet = ('a' to 'z').toList

    def getRandomName(length: Int): String = {
      List
        .fill(length)(alphabet(Random.nextInt(alphabet.length - 1)))
        .mkString
        .capitalize
    }

    List.fill(namesCount)(getRandomName(Random.nextInt(10) + 1))
  }

}

/** Задание №5
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

  class SimpleChangePhoneService(phoneService: SimplePhoneService)
      extends ChangePhoneService {
    override def changePhone(oldPhone: String, newPhone: String): String = {
      val oldPhoneRecord = phoneService.findPhoneNumber(oldPhone)
      if (oldPhoneRecord != null) {
        phoneService.deletePhone(oldPhoneRecord)
      }
      phoneService.addPhoneToBase(newPhone)
      "Ok"
    }
  }

  class PhoneServiceSafety(unsafePhoneService: SimplePhoneService) {
    def findPhoneNumberSafe(num: String): Option[String] = {
      Option(unsafePhoneService.findPhoneNumber(num))
    }

    def addPhoneToBaseSafe(phone: String): Either[String, Unit] = {
      try {
        unsafePhoneService.addPhoneToBase(phone)
        Right()
      } catch {
        case e: Throwable => Left(e.getMessage)
      }
    }

    def deletePhoneSafe(phone: String): Either[String, Unit] = {
      findPhoneNumberSafe(phone) match {
        case Some(_) =>
          unsafePhoneService.deletePhone(phone)
          Right()
        case _ => Left("Phone not found")
      }
    }
  }

  class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety)
      extends ChangePhoneService {
    override def changePhone(oldPhone: String, newPhone: String): String = {
      phoneServiceSafety.deletePhoneSafe(oldPhone) match {
        case Right(_) =>
          phoneServiceSafety.addPhoneToBaseSafe(newPhone) match {
            case Right(_) => "Ok"
            case Left(err) => s"Can't add new phone: $err"
          }
        case Left(err) => s"Can't delete old phone: $err"
      }
    }
  }
}
