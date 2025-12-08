package example

object Main extends App {
  val name = "Alexander Gorshkov"
  val greetings = List("Hello", "Hola", "Ciao")
  for (greeting <- greetings) println(greeting + " Scala! This is " + name)
  for (greeting <- greetings) println(greeting + " Scala! This is " + name.reverse)
}
