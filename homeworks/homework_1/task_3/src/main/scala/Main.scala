object Main {
  def main(args: Array[String]): Unit = {
    val greetingWords = Seq("Hello", "Hola", "Guten tag")

    val myName = "Alex Iarmoshenko"
    val addressees = Seq(myName, myName.reverse)

    for {
      greetingWord <- greetingWords
      name <- addressees
    } {
      val message = getGreeting(greetingWord, name)
      println(message)
    }
  }

  def getGreeting(greetingWord: String, name: String): String = s"$greetingWord Scala! This is $name"
}

