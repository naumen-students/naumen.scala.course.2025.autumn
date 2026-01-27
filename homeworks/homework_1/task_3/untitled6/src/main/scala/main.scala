object Main extends App {
  val name = "Artem"
  val greetings = List("Привет", "Hola", "Guten tag")

  def greetAll(name: String, greetings: List[String]): Unit = {
    greetings.foreach { g =>
      println(s"$g, Скала! Это $name")
    }
  }

  greetAll(name, greetings)
  greetAll(name.reverse, greetings)
}
