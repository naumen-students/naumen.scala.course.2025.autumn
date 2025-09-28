
object Main {
  def main(args: Array[String]): Unit = {
    val str = "Scala! "

    val conststr = "I'm "

    val name = "Anton"


    val strhi = List("Hello ", "Hola ", "Ciao ")


    for (hi <- strhi){
      println(hi + str + conststr + name)
    }

    for (hi <- strhi){
      println(hi + str + conststr + name.reverse)
    }
  }
}