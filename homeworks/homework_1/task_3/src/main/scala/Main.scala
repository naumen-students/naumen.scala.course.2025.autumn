object Main extends App {
  val name = "Anna Zamyslova"
  val greetings = Seq("Hello", "Hola", "Guten tag")
  val reversedName = name.reverse

  greetings.foreach(g => println(s"$g, Scala! It's $name"))
  greetings.foreach(g => println(s"$g, Scala! It's $reversedName"))
}
