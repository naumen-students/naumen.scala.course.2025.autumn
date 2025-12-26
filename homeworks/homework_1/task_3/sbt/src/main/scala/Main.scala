object Main extends App {
  val name = "Kirill Kostogryzov"
  val reverse = name.reverse

  for {
    name <- Seq(name, reverse)
    greeting <- Seq(Greetings.english, Greetings.french, Greetings.spanish)
  } {
    Greetings.sayhello(greeting, name)
  }
}

case class Greeting(word: String)

object Greetings {
  val english = Greeting("Hello")
  val french = Greeting("Salut")
  val spanish = Greeting("Hola")

  def sayhello(greeting: Greeting, name: String): Unit = {
    println(s"${greeting.word} Scala! This is ${name}")
  }
}