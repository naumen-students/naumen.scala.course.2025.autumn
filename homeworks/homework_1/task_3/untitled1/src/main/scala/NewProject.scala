object Main extends App{
  val name: String = "karina"

  val helloMessage = "Hello"


  val str: String = s", Scala! This is"

  val newName = "Karina Akhmatova"

  val str2: String = s"$helloMessage $str ${newName.reverse}"

  val helloMessage2: String = "Привет"

  val printResults = {
    println(s"$helloMessage $str $newName")
    println(s"$helloMessage2 $str $newName")
    println(s"$helloMessage $str  This is ${newName.reverse}")
  }
}