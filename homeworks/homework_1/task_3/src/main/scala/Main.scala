object Main {
  def main(args: Array[String]): Unit = {
    def getGreeting(greetingWord: String, userName: String): String = {
      return s"${greetingWord}, Скала! Это ${userName}"
    }

    val userName: String = "Катя"

    println(getGreeting("Привет", userName))
    println(getGreeting("Bonjour", userName))
    println(getGreeting("Ciao", userName.reverse))
  }
}