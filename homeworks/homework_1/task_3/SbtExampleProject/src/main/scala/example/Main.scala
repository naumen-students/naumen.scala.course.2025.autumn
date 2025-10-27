package example

object Main {
  def main(args: Array[String]): Unit = {
    val greets = List("Hello", "Hola", "Guten tag")
    val name = "Vlad Loparev"

    greeting(greets(0), name)
    println()

    greets.foreach(greet => greeting(greet, name))
    println()

    greets.foreach(greet => greeting_reversed(greet, name))
  }

  def greeting(greet: String, name: String): Unit = {
    println(s"$greet, Scala! This is $name")
  }

  def greeting_reversed(greet: String, name: String): Unit = {
    println(s"$greet, Scala! This is ${name.reverse}")
  }

}