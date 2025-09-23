object Main extends App {
  private val greetings = Array("Hello", "Hola", "Guten tag")
  private val username = "Stanislav Tsaplev"

  for (i  <- 0 to 2)
    println(s"${greetings(i)} Scala! This is ${username}")

  for (i  <- 0 to 2)
    println(s"${greetings(i)} Scala! This is ${username.reverse}")
}