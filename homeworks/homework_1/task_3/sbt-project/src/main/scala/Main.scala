object Main extends App {
  def printMessage(hello: String, name: String) = println(s"$hello Scala! This is $name")

  val myName = "Vitaly Karpov"
  lazy val helloWords = Array("Hello", "Hola", "Bonjour", "Guten tag")

  helloWords.foreach(helloWord => {
    printMessage(helloWord, myName)
    printMessage(helloWord, myName.reverse)
  })
}