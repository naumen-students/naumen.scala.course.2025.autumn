object Main {
  def main(args: Array[String]): Unit = {
    val greetings = List("Hello", "Hola", "Guten tag")

    val myName = "Ivan Fedorov"
    val reversedName = myName.reverse

    val names = List(myName, reversedName)

    for(
      name <- names;
      greeting <- greetings
    ){
      println(s"$greeting Scala! This is $name")
    }
  }
}