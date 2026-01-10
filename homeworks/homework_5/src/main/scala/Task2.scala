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
      override val empty: RadiusVector = RadiusVector(0, 0)

      override def combine(a: RadiusVector, b: RadiusVector): RadiusVector =
        RadiusVector(a.x + b.x, a.y + b.y)
    }
  }

  case class DegreeAngle(angel: Double)
  object DegreeAngle {
    private val Grad = 360.0

    @inline private def wrap(value: Double): Double = {
      val k = math.floor(value / Grad)
      value - k * Grad
    }

    def apply(a: Double): DegreeAngle =
      new DegreeAngle(wrap(a))

    implicit val monoid: Monoid[DegreeAngle] = new Monoid[DegreeAngle] {
      override val empty: DegreeAngle = DegreeAngle(0.0)

      override def combine(x: DegreeAngle, y: DegreeAngle): DegreeAngle =
        DegreeAngle(x.angel + y.angel)
    }
  }

  case class SquareMatrix[A : Monoid](values: ((A, A, A), (A, A, A), (A, A, A)))
  object SquareMatrix {
    implicit def monoid[A: Monoid]: Monoid[SquareMatrix[A]] =
      new Monoid[SquareMatrix[A]] {

        private val M = Monoid[A]

        private def addRow(r1: (A, A, A), r2: (A, A, A)): (A, A, A) = {
          val (a1, a2, a3) = r1
          val (b1, b2, b3) = r2
          (a1 |+| b1, a2 |+| b2, a3 |+| b3)
        }

        override def empty: SquareMatrix[A] = {
          val e = M.empty
          val row = (e, e, e)
          SquareMatrix((row, row, row))
        }

        override def combine(x: SquareMatrix[A], y: SquareMatrix[A]): SquareMatrix[A] = {
          val (xr1, xr2, xr3) = x.values
          val (yr1, yr2, yr3) = y.values

          SquareMatrix((
            addRow(xr1, yr1),
            addRow(xr2, yr2),
            addRow(xr3, yr3)
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
