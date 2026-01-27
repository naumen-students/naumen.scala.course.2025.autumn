package example

object Main extends App {
  val name = "Roman Lyzhin"
  val greetings = List("Hello", "Hola", "Guten Tag")

  def printGreetings(person: String, greetings: List[String]): Unit =
    greetings.foreach(g => println(s"$g Scala! This is $person"))

  printGreetings(name, greetings)
  printGreetings(name.reverse, greetings)
}
