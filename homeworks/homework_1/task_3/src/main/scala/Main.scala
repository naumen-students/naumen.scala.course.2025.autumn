object Main extends App {
  val salutations = Array("Привет", "Hola", "Guten tag")
  val name: String = "Настя Костарева"
  val reverseName: String = name.reverse

  def hello (s : String, name : String) : Unit = println(s"$s Скала! Это $name")

  salutations.foreach { salutation => hello(salutation, name) }
  salutations.foreach { salutation => hello(salutation, reverseName) }
}