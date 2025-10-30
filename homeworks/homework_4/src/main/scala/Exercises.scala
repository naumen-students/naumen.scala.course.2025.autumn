import scala.annotation.tailrec

object Exercises {
    def findSumImperative(items: List[Int], sumValue: Int): (Int, Int) = {
      if (items.isEmpty) return (-1, -1)
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
      (0 until items.length).foldLeft((-1, -1)) { (value, i) =>
        (0 until items.length).foldLeft(value) { (res, j) =>
          if (items(i) + items(j) == sumValue && i != j) (i, j) else res
        }
      }
    }


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
        def recursive(arr: List[(Int, Int)], res: Int): Int = arr match {
          case (value, index) :: tail =>
            val updated = if (value % 2 == 0) value * res + index else - value * res + index
            recursive(tail, updated)
          case Nil => res
        }

        val reversedIndexedList = items.zipWithIndex.map { case(v, i) => (v, i + 1) }.reverse
        recursive(reversedIndexedList, 1)
    }

    def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
        @tailrec
        def recursive(left: Int, right: Int): Option[Int] = {
          if (right < left)
            None
          else {
            val mid = left + (right - left) / 2
            if (items(mid) == value) {
              Some(mid)
            } else if (value < items(mid))
              recursive(left, mid - 1)
            else
              recursive(mid + 1, right)
          }
        }

        recursive(0, items.length - 1)
    }

    def generateNames(namesСount: Int): List[String] = {
        if (namesСount == 0) Nil
        else if (namesСount < 0) throw new Throwable("Invalid namesCount")

        val engUpper = ('A' to 'Z').toArray
        val engLower = ('a' to 'z').toArray
        val rusUpper = ('А' to 'Я') :+ 'Ё'
        val rusLower = ('а' to 'я') :+ 'ё'
        val upper = (engUpper ++ rusUpper).toArray
        val lower = (engLower ++ rusLower).toArray


      def getName(n: Int): String = {
          val firstLetter = upper(n % upper.length).toString
          val tailLen = 1 + ((n / upper.length) % 8)
          val tail = (0 until tailLen).map(i => lower(((n / upper.length) + i) % lower.length)).mkString
          firstLetter + tail
        }

      (0 until namesСount).toList.map(getName)
    }

}


object SideEffectExercise {
    import Utils._
    import scala.util.control.NonFatal

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
        def findPhoneNumberSafe(num: String): Option[String] =
          Option(unsafePhoneService.findPhoneNumber(num))

        def addPhoneToBaseSafe(phone: String): Either[String, Unit] =
          try {
            unsafePhoneService.addPhoneToBase(phone)
            Right(())
          } catch {
            case NonFatal(e) => Left(e.getMessage)
          }

        def deletePhoneSafe(phone: String): Either[String, Unit] =
          try {
            unsafePhoneService.deletePhone(phone)
            Right(())
          } catch {
            case NonFatal(e) => Left(e.getMessage)
          }
    }

    class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
        override def changePhone(oldPhone: String, newPhone: String): String = {
          phoneServiceSafety.findPhoneNumberSafe(oldPhone) match {
            case Some(found) =>
              phoneServiceSafety.deletePhoneSafe(found) match {
                case Left(error) => error
                case Right(_) =>
              phoneServiceSafety.addPhoneToBaseSafe(newPhone) match {
                case Left(err) => err
                case Right(_) => "ok"
              }
            }
            case None =>
              phoneServiceSafety.addPhoneToBaseSafe(newPhone) match {
                case Left(err) => err
                case Right(_) => "ok"
              }
          }

        }
    }
}
