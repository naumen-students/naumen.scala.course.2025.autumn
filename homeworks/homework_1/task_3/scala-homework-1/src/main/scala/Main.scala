object Main extends App {
  private val name = "Алексей Сапан"
  private val greetings = List("Привет", "Hola", "Guten tag")

  println("=== Оригинальные приветствия ===")
  greetings.foreach { greeting =>
    println(s"$greeting, Скала! Это $name")
  }

  println("\n=== Приветствия с перевернутым именем ===")
  private val reversedName = name.reverse
  greetings.foreach { greeting =>
    println(s"$greeting, Скала! Это $reversedName")
  }
}