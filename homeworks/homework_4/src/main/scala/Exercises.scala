import scala.annotation.tailrec
import scala.util.Random

object Exercises {

    def findSumImperative(items: List[Int], sumValue: Int): (Int, Int) = {
        for (i <- items.indices; j <- items.indices if i != j && items(i) + items(j) == sumValue) return (i, j)
        (-1, -1)
    }

    def findSumFunctional(items: List[Int], sumValue: Int): (Int, Int) = {
        items.indices
          .flatMap(i =>
              items.indices.collect {
                  case j if i != j && items(i) + items(j) == sumValue => (i, j)
              }
          )
          .headOption
          .getOrElse((-1, -1))
    }

    def simpleRecursion(items: List[Int], index: Int = 1): Int = items match {
        case Nil => 1
        case head :: tail =>
            val res = simpleRecursion(tail, index + 1)
            val sign = if (head % 2 == 0) 1 else -1
            sign * head * res + index
    }

    def tailRecRecursion(items: List[Int]): Int = {
        1
        @tailrec
        def inner_func(l: List[Int], i: Int, a: Int): Int = {
            l match {
                case head :: tail =>
                    val newAcc =
                        if (head % 2 == 0) {
                            head * a + i
                        } else {
                            - head * a + i
                        }
                    inner_func(tail, i - 1, newAcc)
                case Nil => a
            }
        }
        inner_func(items.reverse, items.length, 1)
    }


    def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
        @tailrec
        def loop(low: Int, high: Int): Option[Int] = {
            if (low > high) None
            else {
                val mid = (low + high) / 2
                val midVal = items(mid)
                if (midVal == value) Some(mid)
                else if (midVal < value) loop(mid + 1, high)
                else loop(low, mid - 1)
            }
        }
        loop(0, items.length - 1)
    }

    def generateNames(namesCount: Int): List[String] = {
        if (namesCount < 0) throw new Throwable("Invalid namesCount")
        val letters = ('a' to 'z').toList
        def randomName(): String = {
            val len = Random.nextInt(5) + 3
            val name = (1 until len).map(_ => letters(Random.nextInt(letters.size))).mkString
            name.capitalize
        }
        List.fill(namesCount)(randomName()).distinct
    }

    def generateRandomList(maxListSize: Int): List[Int] = {
        val listLength = Random.nextInt(Math.max(2, maxListSize - 1)) + 2 // гарантировано >= 2
        List.fill(listLength)(Random.nextInt())
    }
}

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
        def findPhoneNumberSafe(num: String): Option[String] = Option(unsafePhoneService.findPhoneNumber(num))

        def addPhoneToBaseSafe(phone: String): Either[String, Unit] = {
            try {
                unsafePhoneService.addPhoneToBase(phone)
                Right(())
            } catch { case _: Throwable => Left("Invalid phone number") }
        }

        def deletePhoneSafe(phone: String): Either[String, Unit] = {
            try {
                unsafePhoneService.deletePhone(phone)
                Right(())
            } catch { case _: Throwable => Left("Delete failed") }
        }
    }

    class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
        override def changePhone(oldPhone: String, newPhone: String): String = {
            phoneServiceSafety.findPhoneNumberSafe(oldPhone) match {
                case Some(found) =>
                    phoneServiceSafety.deletePhoneSafe(found)
                    phoneServiceSafety.addPhoneToBaseSafe(newPhone) match {
                        case Right(_) => "ok"
                        case Left(err) => s"Add failed: $err"
                    }
                case None => "Old phone not found"
            }
        }
    }
}