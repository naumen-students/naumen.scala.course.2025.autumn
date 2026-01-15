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
    items.zipWithIndex
      .flatMap { case (a, i) =>
        items.zipWithIndex.collect {
          case (b, j) if i != j && a + b == sumValue => (i, j)
        }
      }
      .lastOption
      .getOrElse((-1, -1))
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
    val n = items.length
    val reversed = items.reverse
    @tailrec
    def loop(remaining: List[Int], index: Int, acc: Int): Int = {
      remaining match {
        case head :: tail =>
          if (head % 2 == 0) {
            loop(tail, index - 1, head * acc + index)
          } else {
            loop(tail, index - 1, -head * acc + index)
          }
        case Nil => acc
      }
    }
    if (items.isEmpty) 1 else loop(reversed, n, 1)
  }

  /** Задание №3
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
        val midValue = items(mid)
        if (midValue == value) Some(mid)
        else if (value < midValue) search(low, mid - 1)
        else search(mid + 1, high)
      }
    }

    if (items.isEmpty) None
    else search(0, items.length - 1)
  }

  /** Задание №4
    * Реализуйте функцию, которая генерирует список заданной длинны c именами.
    * Функция должна соответствовать всем правилам функционального программирования.
    *
    * Именем является строка, не содержащая иных символов, кроме буквенных, а также начинающаяся с заглавной буквы.
    */

  def generateNames(namesCount: Int): List[String] = {
    if (namesCount < 0) throw new IllegalArgumentException("Invalid namesCount")
    if (namesCount == 0) return Nil

    val random = new Random
    val uppercase = ('A' to 'Z').toVector
    val lowercase = ('a' to 'z').toVector

    def generateName: String = {
      val nameLength = random.nextInt(8) + 3
      val firstChar = uppercase(random.nextInt(uppercase.length))
      val restChars =
        Vector.fill(nameLength - 1)(lowercase(random.nextInt(lowercase.length)))
      firstChar + restChars.mkString
    }

    @tailrec
    def generateUniqueNames(count: Int, acc: Set[String]): List[String] = {
      if (count <= 0) acc.toList
      else {
        val name = generateName
        if (acc.contains(name)) generateUniqueNames(count, acc)
        else generateUniqueNames(count - 1, acc + name)
      }
    }

    generateUniqueNames(namesCount, Set())
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
        case e: InternalError => Left(e.getMessage)
        case e: Throwable     => Left(s"Unexpected error: ${e.getMessage}")
      }
    }

    def deletePhoneSafe(phone: String): Either[String, Unit] = {
      try {
        findPhoneNumberSafe(phone) match {
          case Some(_) =>
            unsafePhoneService.deletePhone(phone)
            Right(())
          case None => Left("Phone number not found for deletion")
        }
      } catch {
        case e: Throwable => Left(s"Error during deletion: ${e.getMessage}")
      }
    }
  }

  class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety)
      extends ChangePhoneService {
    override def changePhone(oldPhone: String, newPhone: String): String = {
      val oldPhoneCheck = phoneServiceSafety.findPhoneNumberSafe(oldPhone)

      val deleteResult = oldPhoneCheck match {
        case Some(_) => phoneServiceSafety.deletePhoneSafe(oldPhone)
        case None    => Right(())
      }

      val result = for {
        _ <- deleteResult
        _ <- phoneServiceSafety.addPhoneToBaseSafe(newPhone)
      } yield "ok"

      result.fold(
        error => error,
        success => success
      )
    }
  }
}
