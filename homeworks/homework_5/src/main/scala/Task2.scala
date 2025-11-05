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
      def combine(first: RadiusVector, second: RadiusVector): RadiusVector =
        RadiusVector(first.x + second.x, first.y + second.y)
    }
  }
  case class DegreeAngle private (angel: Double)
  object DegreeAngle {
    def apply(a: Double): DegreeAngle = {
      val norm = ((a % 360) + 360) % 360
      new DegreeAngle(norm)
    }
    implicit val monoid: Monoid[DegreeAngle] = new Monoid[DegreeAngle] {
      def empty: DegreeAngle = DegreeAngle(0)
      def combine(first: DegreeAngle, second: DegreeAngle): DegreeAngle = {
        DegreeAngle(first.angel + second.angel)
      }
    }
  }

  case class SquareMatrix[A : Monoid](values: ((A, A, A), (A, A, A), (A, A, A)))
  object SquareMatrix {
    implicit def monoid[A: Monoid]: Monoid[SquareMatrix[A]] = new Monoid[SquareMatrix[A]] {
      private val matrix = implicitly[Monoid[A]]
      def empty: SquareMatrix[A] = {
        val n = matrix.empty
        SquareMatrix((n, n, n), (n, n, n), (n, n, n))
      }

      def combine(first: SquareMatrix[A], second: SquareMatrix[A]): SquareMatrix[A] = {
        def combTriple(t1: (A, A, A), t2: (A, A, A)): (A, A, A) = {
          val (a1, a2, a3) = t1
          val (b1, b2, b3) = t2
          (matrix.combine(a1, b1), matrix.combine(a2, b2), matrix.combine(a3, b3))
        }
        val r1 = combTriple(first.values._1, second.values._1)
        val r2 = combTriple(first.values._2, second.values._2)
        val r3 = combTriple(first.values._3, second.values._3)
        SquareMatrix((r1, r2, r3))
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
