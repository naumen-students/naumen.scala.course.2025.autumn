import cats._
import cats.implicits._

/*
  Задание №2
  Всё просто, для каждого кейс класса необходимо описать логику его сложения.
  Радиус-вектор должен складываться, как и любой другой вектор.
  GradeAngle всегда выражает угол [0, 360).
  SquareMatrix просто сложение квадратных матриц
 */
object Task2 extends App {
  case class RadiusVector(x: Int, y: Int)

  object RadiusVector {
    implicit val monoid: Monoid[RadiusVector] = new Monoid[RadiusVector] {
      override def empty: RadiusVector = RadiusVector(0, 0)

      override def combine(a: RadiusVector, b: RadiusVector): RadiusVector =
        RadiusVector(a.x + b.x, a.y + b.y)
    }
  }

  case class DegreeAngle(angel: Double)

  object DegreeAngle {
    def apply(a: Double): DegreeAngle = new DegreeAngle(((a % 360) + 360) % 360)

    implicit val monoid: Monoid[DegreeAngle] = new Monoid[DegreeAngle] {
      override def empty: DegreeAngle = DegreeAngle(0.0)

      override def combine(a: DegreeAngle, b: DegreeAngle): DegreeAngle = DegreeAngle(a.angel + b.angel)
    }
  }

  case class SquareMatrix[A: Monoid](values: ((A, A, A), (A, A, A), (A, A, A)))

  object SquareMatrix {
    implicit def monoid[A: Monoid]: Monoid[SquareMatrix[A]] = new Monoid[SquareMatrix[A]] {
      private val M = implicitly[Monoid[A]]

      override def empty: SquareMatrix[A] = SquareMatrix((
        (M.empty, M.empty, M.empty),
        (M.empty, M.empty, M.empty),
        (M.empty, M.empty, M.empty)
      ))

      override def combine(x: SquareMatrix[A], y: SquareMatrix[A]): SquareMatrix[A] = {
        val ((a11, a12, a13), (a21, a22, a23), (a31, a32, a33)) = x.values
        val ((b11, b12, b13), (b21, b22, b23), (b31, b32, b33)) = y.values
        SquareMatrix((
          (M.combine(a11, b11), M.combine(a12, b12), M.combine(a13, b13)),
          (M.combine(a21, b21), M.combine(a22, b22), M.combine(a23, b23)),
          (M.combine(a31, b31), M.combine(a32, b32), M.combine(a33, b33))
        ))
      }
    }
  }
}
