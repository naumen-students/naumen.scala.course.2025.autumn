val myName = "Danila"
val baseHello = "Hello"
val greet = (word: String, name: String) => s"$word, Scala! It is $name"
println(greet(baseHello, myName))

val otherHellos = List("Hola", "Guten tag")
otherHellos.foreach(h => println(greet(h, myName)))

val recipients = List("Ian Ivanov", "Maria Sidorova", myName)
val reverse: String => String = _.reverse
recipients.foreach(n => println(greet(baseHello, reverse(n))))