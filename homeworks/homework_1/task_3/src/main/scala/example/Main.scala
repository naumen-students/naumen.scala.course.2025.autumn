object GreetingApp extends App {
  val name = "Sergei Dagaev"
  val greetings = List("Hello", "Holla", "Guten tag")

  def greet(greeting: String, person: String): Unit =
    println(s"$greeting, Scala! This is $person")

  greetings.foreach(greet(_, name))

  greetings.foreach(greet(_, name.reverse))
}
