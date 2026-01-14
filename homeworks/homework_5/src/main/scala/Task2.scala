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
      def combine(v1: RadiusVector, v2: RadiusVector): RadiusVector =
        RadiusVector(v1.x + v2.x, v1.y + v2.y)
    }
  }
  case class DegreeAngle(angel: Int)
  object DegreeAngle {
    implicit val monoid: Monoid[DegreeAngle] = new Monoid[DegreeAngle] {
      def empty: DegreeAngle = DegreeAngle(0)
      def combine(a: DegreeAngle, b: DegreeAngle): DegreeAngle = {
        val sum = (a.angel % 360) + (b.angel % 360)
        val result = (sum + 360) % 360
        DegreeAngle(result)
      }
    }
  }

  case class SquareMatrix[A : Monoid](values: ((A, A, A), (A, A, A), (A, A, A)))
  object SquareMatrix {
    implicit def monoid[A: Monoid]: Monoid[SquareMatrix[A]] = new Monoid[SquareMatrix[A]] {
      def empty: SquareMatrix[A] = {
        val zero = Monoid[A].empty
        SquareMatrix((
          (zero, zero, zero),
          (zero, zero, zero),
          (zero, zero, zero)
        ))
      }

      def combine(a: SquareMatrix[A], b: SquareMatrix[A]): SquareMatrix[A] = {
        val monoidA = Monoid[A]

        def combineRow(row1: (A, A, A), row2: (A, A, A)): (A, A, A) =
          (
            monoidA.combine(row1._1, row2._1),
            monoidA.combine(row1._2, row2._2),
            monoidA.combine(row1._3, row2._3)
          )

        SquareMatrix((
          combineRow(a.values._1, b.values._1),
          combineRow(a.values._2, b.values._2),
          combineRow(a.values._3, b.values._3)
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
