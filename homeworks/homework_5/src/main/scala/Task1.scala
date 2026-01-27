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
*/
object Task1 {

  trait Show[-A] {
    def show(a: A): String
  }
  sealed trait Cat { def name: String }
  final case class VeryLittleCat(name: String) extends Cat
  final case class LittleCat(name: String)     extends Cat
  final case class NormalCat(name: String)     extends Cat
  final case class BigCat(name: String)        extends Cat
  final case class VeryBigCat(name: String)    extends Cat

  sealed trait Box[+A]
  case object EmptyBox extends Box[Nothing]
  final case class BoxWith[+A](value: A) extends Box[A]

  object ShowInstance {
    implicit val catShow: Show[Cat] = new Show[Cat] {
      def show(a: Cat): String = a match {
        case VeryLittleCat(name) => s"очень маленький кот $name"
        case LittleCat(name)     => s"маленький кот $name"
        case NormalCat(name)     => s"кот $name"
        case BigCat(name)        => s"большой кот $name"
        case VeryBigCat(name)    => s"очень большой кот $name"
      }
    }

    implicit def boxShow[A](implicit ev: Show[A]): Show[Box[A]] = new Show[Box[A]] {
      def show(b: Box[A]): String = b match {
        case BoxWith(value) => s"${ev.show(value)} в коробке"
        case EmptyBox       => "пустая коробка"
      }
    }
    implicit val emptyBoxShow: Show[EmptyBox.type] = new Show[EmptyBox.type] {
      def show(b: EmptyBox.type): String = "пустая коробка"
    }
  }

  object ShowSyntax {
    implicit class ShowOps[A](val a: A) extends AnyVal {
      def show(implicit show: Show[A]): String = show.show(a)
    }
  }
}
