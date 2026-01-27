object Main extends App {
  val greetings = List("Привет", "Hi", "Hola")

  val name = "Владимир Бойко"

  val names = List(name, name.reverse)

  for (
    name <- names;
    greeting <- greetings
  ) {
    println(s"$greeting, Скала! Это $name")
  }
}