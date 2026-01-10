/*
  Задание №1
  В задание уже описан тайп класс и синтакс для него.
  Вам необходимо в объекте ShowInstance описать инстансы тайп класса
  для типа Cat и Box.
  Тип Cat, в соответствии с тем, какого конкретно наследника этого типа мы хотим показать,
  должен отображаться следующим образом:
  VeryLittleCat - очень маленький кот его_имя
  LittleCat - маленький кот его_имя
  NormalCat - кот его_имя
  BigCat - большой кот его_имя
  VeryBigCat - очень большой кот его_имя

  Если кот будет в коробке, то к тому, что должно выводиться для кота
  необходимо добавить "в коробке". Если коробка пустая, то выводить "пустая коробка"

  В тестах можно всегда более точно посмотреть фразы.
 */
object Task1 extends App {
  // Обратите внимание, что здесь type class параматрезирован контрвариантно
  // благодаря этому инстанс Show[Cat] мы можем применить, например, к BigCat.
  // Однако, это может быть неудобно, так как нам для каждого нового наследника придётся менять
  // реализацию Show[Cat]. Из-за этого практически все библиотеки, которые предоставляют свои
  // тайп классы для их использования пользователем, делают тайп классы инвариантными по параметру.
  // Можете написать две реализации (при -А и А) и сравнить их.
  trait Show[-A] {
    def show(a: A): String
  }

  sealed trait Cat {
    def name: String
  }
  case class VeryLittleCat(name: String) extends Cat
  case class LittleCat(name: String) extends Cat
  case class NormalCat(name: String) extends Cat
  case class BigCat(name: String) extends Cat
  case class VeryBigCat(name: String) extends Cat

  sealed trait Box[+A] {
    def value: A
  }
  case class BoxWith[+A](value: A) extends Box[A]
  case object EmptyBox extends Box[Nothing] {
    override def value: Nothing = throw new Exception("Empty box!")
  }

  object ShowInstance {

    private def catPhrase(cat: Cat): String = cat match {
      case _: VeryLittleCat => "очень маленький кот"
      case _: LittleCat     => "маленький кот"
      case _: NormalCat     => "кот"
      case _: BigCat        => "большой кот"
      case _: VeryBigCat    => "очень большой кот"
    }

    implicit val catShow: Show[Cat] = new Show[Cat] {
      override def show(cat: Cat): String =
        s"${catPhrase(cat)} ${cat.name}"
    }

    implicit def boxShow[A: Show]: Show[Box[A]] = new Show[Box[A]] {
      private val showA = implicitly[Show[A]]

      override def show(box: Box[A]): String = box match {
        case EmptyBox       => "пустая коробка"
        case BoxWith(value) => s"${showA.show(value)} в коробке"
      }
    }
  }

  object ShowSyntax {
    implicit class ShowOps[A](val a: A) {
      def show(implicit show: Show[A]): String = show.show(a)
    }
  }
}