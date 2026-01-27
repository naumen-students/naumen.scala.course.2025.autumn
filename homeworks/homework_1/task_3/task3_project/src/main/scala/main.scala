object Main extends App{
  val name: String = "Albina"

  val str = s"Привет, Скала! Это $name"
  println(str)

  def renameHello(s: String): String = {
    val newStr = str.replace("Привет", s)
    newStr
  }

  val str1 = renameHello("Hello")
  println(str1)


  val str2 = renameHello("Hola")
  println(str2)

  val strName = str.replace(s"$name", s"${name.reverse}")
  println(strName)


}

