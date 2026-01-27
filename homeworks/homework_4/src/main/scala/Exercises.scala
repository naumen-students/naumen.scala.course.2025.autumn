import Utils.{ChangePhoneService, SimplePhoneService}

import scala.annotation.tailrec
import scala.util.{Failure, Random, Success, Try}


object Exercises {

  /**
   * Задание №1
   *
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

  @tailrec
  def findSumFunctional(items: List[Int], sumValue: Int): (Int, Int) = {
    @tailrec
    def findItemFunctional(items: List[Int], value: Int): Int = {
      if (items.isEmpty)
        return -1

      if (items.last == value)
        items.length - 1
      else
        findItemFunctional(items.init, value)
    }

    if (items.isEmpty)
      return (-1, -1)

    val index = findItemFunctional(items.init, sumValue - items.last)
    if (index >= 0)
      (items.length - 1, index)
    else
      findSumFunctional(items.init, sumValue)
  }


  /**
   * Задание №2
   *
   * Дана рекурсивная функция simpleRecursion.
   * Перепишите ее так, чтобы получилась хвостовая рекурсивная функция.
   *
   * Для прохождения теста на большое количество элементов в списке
   * используйте аннотацию @tailrec к вашей функции.
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
  def tailRecRecursion(items: List[Int], acc: Int = 1): Int = {
    if (items.isEmpty)
      return acc

    tailRecRecursion(items.init,
      acc * ((if (items.last % 2 == 0) 1 else -1) * items.last) + items.length)
  }

  /**
   * Задание №3
   *
   * Реализуйте алгоритм бинарного поиска, который соответствует всем правилам функционального программирования.
   * Необходимо возвращать индекс соответствующего элемента в массиве
   * Если ответ найден, то возвращается Some(index), если нет, то None
   */

  def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
    @tailrec
    def binarySearchRec(left: Int, right: Int): Option[Int] = {
      if (left > right)
        return None

      val middle = left + (right - left) / 2
      if (value < items(middle))
        binarySearchRec(left, middle - 1)
      else if (value > items(middle))
        binarySearchRec(middle + 1, right)
      else
        Some(middle)
    }

    binarySearchRec(0, items.length - 1)
  }

  /**
   * Задание №4
   *
   * Реализуйте функцию, которая генерирует список заданной длины с именами.
   * Функция должна соответствовать всем правилам функционального программирования.
   *
   * Именем является строка, не содержащая иных символов, кроме буквенных, а также начинающаяся с заглавной буквы.
   */

  def generateNames(namesCount: Int): List[String] = {
    if (namesCount < 0) throw new Throwable("Invalid namesCount")

    def generateName(): String = {
      def generateNameRec(length: Int): String = {
        if (length > 0) {
          val index = Random.nextInt(58)
          val char =
            if (index < 26)
              ('a'.toInt + index).toChar        // latin a
            else
              ('а'.toInt + index - 26).toChar   // cyrillic а

          char + generateNameRec(length - 1)
        } else
          ""
      }

      val index = Random.nextInt(58)
      val firstChar =
        if (index < 26)
          ('A'.toInt + index).toChar        // latin A
        else
          ('А'.toInt + index - 26).toChar   // cyrillic А

      val nameLength = 2 + Random.nextInt(10)
      firstChar + generateNameRec(nameLength)
    }

    def generateNamesRec(namesCount: Int, names: List[String]): List[String] = {
      if (namesCount > 0) {
        val name = generateName()
        if (names.contains(name))
          generateNamesRec(namesCount, names)

        generateNamesRec(namesCount - 1, name :: names)
      } else
        names
    }

    generateNamesRec(namesCount, List[String]())
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

  class SafePhoneService(unsafePhoneService: SimplePhoneService) {
    def findPhoneNumberSafe(phoneNumber: String): Option[String] =
      Option(unsafePhoneService.findPhoneNumber(phoneNumber))

    def addPhoneToBaseSafe(phoneNumber: String): Try[Unit] = {
      try {
        unsafePhoneService.addPhoneToBase(phoneNumber)
        Success()
      }
      catch {
        case e: InternalError => Failure(e)
      }
    }

    def deletePhoneSafe(phoneNumber: String): Try[Unit] = {
      try {
        unsafePhoneService.deletePhone(phoneNumber)
        Success()
      }
      catch {
        case e: Throwable => Failure(e)
      }
    }
  }

  class ChangePhoneServiceSafe(safePhoneService: SafePhoneService) extends ChangePhoneService {
    override def changePhone(oldPhone: String, newPhone: String): String = {
      safePhoneService.findPhoneNumberSafe(oldPhone) match {
        case Some(value) => safePhoneService.deletePhoneSafe(value) match {
          case Failure(e) => e.toString
        }
      }
      safePhoneService.addPhoneToBaseSafe(newPhone) match {
        case Failure(e) => e.toString
      }
      "ok"
    }
  }
}
