object Main extends App {
  var name = "Алексей Павлов"
  var greetings = List("Привет", "Hola", "Guten tag")

  def createGreeting(g: String, n: String): String = {
    s"$g, Скала! Это $n";
  }

  println(createGreeting(greetings(0), name))
  println(createGreeting(greetings(1), name))

  def reverse(word: String): String = {
    word.split(" ").map(_.reverse).reverse.mkString(" ")
  }

  println(createGreeting(greetings(0), reverse(name)))
}