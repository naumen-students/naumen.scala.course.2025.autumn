object Main extends App{
  def print(name: String, words: List[String]): Unit =
    words.foreach(word => println(s"$word Scala! This is $name"))

  val name = "Anton Novikov"
  val word = List("Hello")
  print(name, word)

  val words = List("Halo", "Guten Tag")

  print(name, words)

  print(name.reverse, word ++ words)
}