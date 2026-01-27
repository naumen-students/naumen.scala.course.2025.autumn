package hello

object Main extends App {
  val myName = "Антон"
  val greetings = List("Привет", "Hola", "Guten tag")
  val baseMessage = ", Скала! Это "

  def reverseString(str: String): String = str.reverse

  def printGreetings(name: String, greetingList: List[String]): Unit = {
    greetingList.foreach { greeting =>
      println(greeting + baseMessage + name)
    }
  }

  def processAllGreetings(name: String): Unit = {
    val nameVariants = List(name, reverseString(name))
    val variantLabels = List("Оригинальное", "Перевернутое")

    nameVariants.zip(variantLabels).foreach { case (nameVariant, label) =>
      println(s"--- $label имя ---")
      printGreetings(nameVariant, greetings)
    }
  }

  processAllGreetings(myName)
}
