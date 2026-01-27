import scala.annotation.tailrec

object Exercises {

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
    val n = items.length
    (0 until n)
      .flatMap(i => (0 until n).map(j => (i, j)))
      .foldLeft((-1, -1)) { case (acc, (i, j)) =>
        if (i != j && items(i) + items(j) == sumValue) (i, j) else acc
      }
  }

  def simpleRecursion(items: List[Int], index: Int = 1): Int = {
    items match {
      case head :: tail =>
        if (head % 2 == 0) head * simpleRecursion(tail, index + 1) + index
        else -1 * head * simpleRecursion(tail, index + 1) + index
      case _ => 1
    }
  }

  def tailRecRecursion(items: List[Int]): Int = {
    @tailrec
    def loop(rev: List[Int], idx: Int, acc: Int): Int = rev match {
      case h :: t =>
        val next = if (h % 2 == 0) h * acc + idx else -h * acc + idx
        loop(t, idx - 1, next)
      case Nil => acc
    }
    val n = items.length
    loop(items.reverse, n, 1)
  }

  def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
    @tailrec
    def go(lo: Int, hi: Int): Option[Int] =
      if (lo > hi) None
      else {
        val mid = lo + (hi - lo) / 2
        val m = items(mid)
        if (m == value) Some(mid)
        else if (m < value) go(mid + 1, hi)
        else go(lo, mid - 1)
      }
    if (items.isEmpty) None else go(0, items.length - 1)
  }

  def generateNames(namesСount: Int): List[String] = {
    if (namesСount < 0) throw new Throwable("Invalid namesCount")
    def toLetters(k: Int): String = {
      @tailrec
      def build(n: Int, acc: List[Char]): List[Char] =
        if (n < 26) (('a' + n).toChar :: acc)
        else {
          val q = n / 26
          val r = n % 26
          build(q - 1, (('a' + r).toChar :: acc))
        }
      val raw = build(k, Nil).mkString
      raw.head.toUpper + raw.tail
    }
    List.tabulate(namesСount)(toLetters)
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
    def findPhoneNumberSafe(num: String): Option[String] =
      Option(unsafePhoneService.findPhoneNumber(num))

    def addPhoneToBaseSafe(phone: String): Either[String, Unit] =
      try { unsafePhoneService.addPhoneToBase(phone); Right(()) }
      catch { case e: Throwable => Left(Option(e.getMessage).getOrElse("addPhone failed")) }

    def deletePhone(phone: String): Either[String, Unit] =
      try { unsafePhoneService.deletePhone(phone); Right(()) }
      catch { case e: Throwable => Left(Option(e.getMessage).getOrElse("deletePhone failed")) }
  }

  class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
    override def changePhone(oldPhone: String, newPhone: String): String = {
      phoneServiceSafety.findPhoneNumberSafe(oldPhone).foreach(ph => phoneServiceSafety.deletePhone(ph))
      phoneServiceSafety.addPhoneToBaseSafe(newPhone) match {
        case Right(_) => "ok"
        case Left(err) => err
      }
    }
  }
}
