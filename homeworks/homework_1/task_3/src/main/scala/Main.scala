object Main extends App {
  val name = "Амир"
  val reversedName = name.reverse

  def greetingTemplate(greeting: String, name: String): String =
    s"$greeting, Скала! Это $name"

  def printGreetings(userName: String): Unit = {
    println(userName)
    val combinations = for {
      greet <- List("Привет", "Hola", "Hi")
    } yield greetingTemplate(greet, userName)
    combinations.foreach(println)
  }

  printGreetings(name)
  printGreetings(reversedName)
}