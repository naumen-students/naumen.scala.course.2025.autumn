object Main extends App {
  val myName = "Alexey Punenko"
  val greetings = List("Hello", "Hola", "Guten tag")
  val names = List("John Smith", "Cayde-6", "Big Boss")

  def greetingPhrase(greet: String, who: String) =
    s"$greet, Scala!, This is $who"

  def reverseName(name: String): String =
    name.reverse

  val reversedNames: List[String] = names.map(reverseName)

  def printGreetings(gs: List[String], names: List[String]): Unit =
    for (g <- gs; n <- names) println(greetingPhrase(g, n))

  printGreetings(greetings.take(1), List(myName))
  printGreetings(greetings, List(myName))
  printGreetings(greetings, reversedNames)
}