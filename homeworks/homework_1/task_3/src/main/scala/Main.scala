object Main extends App {
  val myName = "Егор"
  val greetings = List("Привет", "Hola", "Guten tag")

  greetings.foreach { greeting =>
    println(s"$greeting, Скала! Это $myName")
  }

  val reversedMyName = myName.reverse
  greetings.foreach { greeting =>
    println(s"$greeting, Скала! Это $reversedMyName")
  }
}