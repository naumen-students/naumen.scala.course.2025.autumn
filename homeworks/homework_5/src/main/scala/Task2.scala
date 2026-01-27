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

      override def combine(first: RadiusVector, second: RadiusVector): RadiusVector =
        RadiusVector(first.x + second.x, first.y + second.y)
    }
  }
  case class DegreeAngle(angel: Double)
  object DegreeAngle {
    def apply(angle: Double): DegreeAngle = new DegreeAngle(((angle % 360) + 360) % 360)

    implicit val monoid: Monoid[DegreeAngle] = new Monoid[DegreeAngle] {
      override def empty: DegreeAngle = DegreeAngle(0)

      override def combine(x: DegreeAngle, y: DegreeAngle): DegreeAngle ={
        DegreeAngle(x.angel + y.angel)
      }
    }
  }

  case class SquareMatrix[A : Monoid](values: ((A, A, A), (A, A, A), (A, A, A)))
  object SquareMatrix {
    implicit def monoid[A: Monoid]: Monoid[SquareMatrix[A]] = new Monoid[SquareMatrix[A]] {
      override def empty: SquareMatrix[A] = {
        val monoidEmpty = Monoid[A].empty
        SquareMatrix(
          (monoidEmpty, monoidEmpty, monoidEmpty),
          (monoidEmpty, monoidEmpty, monoidEmpty),
          (monoidEmpty, monoidEmpty, monoidEmpty)
        )
      }

      override def combine(x: SquareMatrix[A], y: SquareMatrix[A]): SquareMatrix[A] = {
        def combineElements(first: A, second: A): A = Monoid[A].combine(first, second)

        SquareMatrix(
          (
            combineElements(x.values._1._1, y.values._1._1),
            combineElements(x.values._1._2, y.values._1._2),
            combineElements(x.values._1._3, y.values._1._3)
          ),
          (
            combineElements(x.values._2._1, y.values._2._1),
            combineElements(x.values._2._2, y.values._2._2),
            combineElements(x.values._2._3, y.values._2._3)
          ),
          (
            combineElements(x.values._3._1, y.values._3._1),
            combineElements(x.values._3._2, y.values._3._2),
            combineElements(x.values._3._3, y.values._3._3)
          )
        )
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
