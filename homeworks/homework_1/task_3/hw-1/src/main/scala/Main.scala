object Main extends App{
  def writeInConsoleGreeting(welcomeSpeech: String, name: String): Unit = println(
    s"$welcomeSpeech Scala! This is $name"
  )

  val name = "Stas"
  val reverseName = name.reverse
  val listWelcomeSpeeches = List("Hello", "Hola", "Guten tag")

  for (welcomeSpeech <- listWelcomeSpeeches)
    writeInConsoleGreeting(welcomeSpeech, name)

  for (welcomeSpeech <- listWelcomeSpeeches)
    writeInConsoleGreeting(welcomeSpeech, reverseName)

}
