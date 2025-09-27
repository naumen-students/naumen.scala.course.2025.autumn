
//TIP To <b>Run</b> code, press <shortcut actionId="Run"/> or click the <icon src="AllIcons.Actions.Execute"/> icon in the gutter.
object Main extends App {
  // Программа вывода приветственного сообщения

  // Список приветственных слов:
  val welcoming_speech1: String = "Hallo"
  val welcoming_speech2: String = "Guten Morgen"
  val welcoming_speech3: String = "Gute Nacht"

  // Выберите приветственное слово из Списка
  var welcoming_speech: String = welcoming_speech1

  // Введите Ваши имя и фамилию
  val name: String = "Wladimir Kokscharow"

  def greeting : Unit = println (
    welcoming_speech + ", Scala! Das ist " + name + "."
  ) // Функция вывода приветственного сообщения

  // Для вывода приветственного сообщения введите в терминале "greeting"
  greeting

  // Выберите другое приветственное слово из Списка
  welcoming_speech = welcoming_speech2

  // Для вывода приветственного сообщения введите в терминале "greeting"
  greeting

  // Выберите другое приветственное слово из Списка
  welcoming_speech = welcoming_speech3

  // Для вывода приветственного сообщения введите в терминале "greeting"
  greeting

  def name_reverse = (for(i <- name.length - 1 to 0 by -1)
    yield name(i)).mkString // Функция преобразования стандартной формы записи имени и фамилии в реверсивную

  def greetings_reverse : Unit = println (
    welcoming_speech1 + ", Scala! Das ist " + name_reverse + ".\n" +
      welcoming_speech2 + ", Scala! Das ist " + name_reverse + ".\n" +
      welcoming_speech3 + ", Scala! Das ist " + name_reverse + "."
  ) // Функция вывода вариантов приветственных сообщений с реверсивной формой записи фамилии и имени

  // Для вывода вариантов приветственных сообщений с реверсивной формой записи фамилии и имени введите в терминале "greetings_reverse"
  greetings_reverse
}

