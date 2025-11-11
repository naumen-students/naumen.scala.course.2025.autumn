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
      def empty: RadiusVector = RadiusVector(0, 0)
      def combine(x: RadiusVector, y: RadiusVector): RadiusVector =
        RadiusVector(x.x + y.x, x.y + y.y)
    }
  }
  case class DegreeAngle(angel: Double)
  object DegreeAngle {
    def apply(a: Double): DegreeAngle = {
      val norm = ((a % 360) + 360) % 360
      new DegreeAngle(norm)
    }
    implicit val monoid: Monoid[DegreeAngle] = new Monoid[DegreeAngle] {
      def empty: DegreeAngle = DegreeAngle(0)
      def combine(a: DegreeAngle, b: DegreeAngle): DegreeAngle = {
        DegreeAngle(a.angel + b.angel)
      }
    }
  }

  case class SquareMatrix[A : Monoid](values: ((A, A, A), (A, A, A), (A, A, A)))
  object SquareMatrix {
    implicit def monoid[A: Monoid]: Monoid[SquareMatrix[A]] = new Monoid[SquareMatrix[A]] {
      def empty: SquareMatrix[A] = {
        val zero = Monoid[A].empty
        SquareMatrix(((zero, zero, zero), (zero, zero, zero), (zero, zero, zero)))
      }
      def combine(x: SquareMatrix[A], y: SquareMatrix[A]): SquareMatrix[A] = {
        val ((x11, x12, x13), (x21, x22, x23), (x31, x32, x33)) = x.values
        val ((y11, y12, y13), (y21, y22, y23), (y31, y32, y33)) = y.values
        SquareMatrix((
          (x11 |+| y11, x12 |+| y12, x13 |+| y13),
          (x21 |+| y21, x22 |+| y22, x23 |+| y23),
          (x31 |+| y31, x32 |+| y32, x33 |+| y33)
        ))
      }
    }
  }

  val radiusVectors = Vector(RadiusVector(0, 0), RadiusVector(0, 1), RadiusVector(-1, 1))
  Monoid[RadiusVector].combineAll(radiusVectors) // RadiusVector(-1, 2)

  val gradeAngles = Vector(DegreeAngle(380), DegreeAngle(60), DegreeAngle(30))
  Monoid[DegreeAngle].combineAll(gradeAngles) // GradeAngle(90)

  val matrixes = Vector(
    SquareMatrix(
      (
        (1, 2, 3),
        (4, 5, 6),
        (7, 8, 9)
      )
    ),
    SquareMatrix(
      (
        (-1, -2, -3),
        (-3, -4, -5),
        (-7, -8, -9)
      )
    )
  )
  Monoid[SquareMatrix[Int]].combineAll(matrixes)
  //  [0, 0, 0]
  //  |1, 1, 1|
  //  [0, 0, 0]
}