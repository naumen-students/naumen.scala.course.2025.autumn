object Main extends App {
    def makeHello(hs: List[String], name: String = "Nosa Sergey"): List[String] = {
        val x = hs.foldRight(Nil: List[String])((x,xs) => s"$x Scala! This is ${name.reverse}"  :: xs)
        x
    }
    val hs = List("Hello", "Hola", "Guten tag")
    val xs = makeHello(hs)
    for {
        x <- xs
    } println (x)
}

