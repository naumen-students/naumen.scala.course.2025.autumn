package example

object Main {
  @main def run(): Unit =
    val greetings = List("Привет", "Hello", "Ni Hao")
    val name = "Андрей Киселев"
    val nameReversed = name.reverse

    def printGreeting(greeting: String, name: String): Unit = {
      val template = s"${greeting}, Скала! Это ${name}"
      println(template)
    }

    for (greeting <- greetings) {
      printGreeting(greeting, name)
    }

    println(nameReversed)

    printGreeting(greetings.head, nameReversed)
}
