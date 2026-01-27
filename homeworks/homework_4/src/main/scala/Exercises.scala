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
    val result = for {
      i <- items.indices
      j <- items.indices
      if items(i) + items(j) == sumValue && i != j
    } yield (i, j)

    result.lastOption.getOrElse((-1, -1))
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
    def loop(items: List[Int], index: Int, value: Int): Int = {
      items match {
        case head :: tail =>
          val head2 = if (head % 2 == 0) head else -head
          loop(tail, index - 1, head2 * value + index)
        case _ => value
      }
    }

    loop(items = items.reverse, index = items.length, value = 1)
  }

  /**
   * Задание №3
   * Реализуйте алгоритм бинарного поиска, который соответсвует всем правилам функционального программирования.
   * Необходимо возвращать индекс соответствующего элемента в массиве
   * Если ответ найден, то возвращается Some(index), если нет, то None
   */

  def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
    @tailrec
    def loop(low: Int, high: Int): Option[Int] = {
      if (low > high) {
        None
      }
      else {
        val mid = (low + high) / 2
        if (items(mid) == value) {
          Some(mid)
        }
        else if (items(mid) < value) {
          loop(mid + 1, high)
        }
        else {
          loop(low, mid - 1)
        }
      }
    }

    loop(0, items.length - 1)
  }

  /**
   * Задание №4
   * Реализуйте функцию, которая генерирует список заданной длинны c именами.
   * Функция должна соответствовать всем правилам функционального программирования.
   *
   * Именем является строка, не содержащая иных символов, кроме буквенных, а также начинающаяся с заглавной буквы.
   */

  def generateNames(namesСount: Int): List[String] = {
    if (namesСount < 0)
      throw new Throwable("Invalid namesCount")
    else {
      val upperLetter = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      val lowerLetter = "abcdefghijklmnopqrstuvwxyz"
      val nameLength = Random.nextInt(12) + 2
      List.fill(namesСount)(upperLetter(Random.nextInt(upperLetter.length)) + (0 until nameLength).map(_ =>
        lowerLetter(Random.nextInt(lowerLetter.length))).mkString)
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
    def findPhoneNumberSafe(num: String): Option[String] = Option(unsafePhoneService.findPhoneNumber(num))

    def addPhoneToBaseSafe(phone: String): Option[String] = {
      try {
        unsafePhoneService.addPhoneToBase(phone)
        None
      } catch {
        case _: Throwable => Option("Invalid phone string")
      }
    }

    def deletePhone(phone: String): Option[String] = {
      if (unsafePhoneService.findPhoneNumber(phone) != null) {
        unsafePhoneService.deletePhone(phone)
        None
      }
      else {
        Option("Phone not found")
      }
    }
  }

  class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
    override def changePhone(oldPhone: String, newPhone: String): String = {
      phoneServiceSafety.findPhoneNumberSafe(oldPhone) match {
        case Some(_) => phoneServiceSafety.deletePhone(oldPhone) match {
          case Some(error) => error
          case None => phoneServiceSafety.addPhoneToBaseSafe(newPhone) match {
            case None => "ok"
            case Some(error) => error
          }
        }
        case None => phoneServiceSafety.addPhoneToBaseSafe(newPhone) match {
          case None => "ok"
          case Some(error) => error
        }
      }
    }
  }
}
