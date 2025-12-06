package homework1

object Main extends App {
  val myName = "Гусева Мария"
  val greetingsList = List("Приветик", "Aloha", "Ciao")

  def printGreeting(prefix: String, name: String): Unit = {
    println(s"$prefix, Скала! Это $name")
  }
  printGreeting("Привет", myName)
  greetingsList.foreach(prefix => printGreeting(prefix, myName))
  greetingsList.foreach(prefix => printGreeting(prefix, myName.reverse))
}