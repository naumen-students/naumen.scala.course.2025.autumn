
object Main {
  def main(args: Array[String]): Unit = {

    val names = List("Makar Pavletsov", "Makar Pavletsov".reverse)
    val greetings = List("Hello", "Guten Tag", "Hei", "Hola")
    names.foreach(name => greetings.foreach(greet => println(s"$greet Scala! This is $name")))
    }
  }

