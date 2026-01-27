object Main extends App {

  val myName = "Kirill Tsupyshtanov"
  val greetings = List("Hello", "Hola", "Guten Tag")
  val baseMessage = "Scala! This is"
  
  def generateGreeting(greeting: String, name: String): String = {
    s"$greeting $baseMessage $name"
  }
  
  def printGreetings(name: String, title: String): Unit = {
    println(s"\n--- $title ---")
    greetings.foreach { greeting =>
      println(generateGreeting(greeting, name))
    }
  }
  
  println("=== Начало выполнения ===")
  
  val originalGreeting = generateGreeting("Hello", myName)
  println(originalGreeting)
  
  printGreetings(myName, "Разные приветствия")
  
  val reversedName = myName.reverse
  printGreetings(reversedName, "С перевернутым именем")
  
  println("\n--- Функциональный подход ---")
  val nameVariants = Map(
    "Оригинальное имя" -> myName,
    "Перевернутое имя" -> reversedName
  )
  
  nameVariants.foreach { case (description, name) =>
    println(s"\n$description:")
    greetings.map(generateGreeting(_, name)).foreach(println)
  }
  
  println("\n=== Завершение выполнения ===")
}
