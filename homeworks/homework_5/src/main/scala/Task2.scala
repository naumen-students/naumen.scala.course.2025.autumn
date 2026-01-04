import cats.Monoid
import cats.implicits.catsSyntaxSemigroup

object Task2 {
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
    implicit val monoid: Monoid[DegreeAngle] = new Monoid[DegreeAngle] {
      override def empty: DegreeAngle = DegreeAngle(0.0)
      override def combine(a: DegreeAngle, b: DegreeAngle): DegreeAngle =
        DegreeAngle(((a.angel + b.angel) % 360 + 360) % 360)
    }
  }

  case class SquareMatrix[A : Monoid](values: ((A, A, A), (A, A, A), (A, A, A)))
  object SquareMatrix {
    implicit def monoid[A: Monoid]: Monoid[SquareMatrix[A]] = new Monoid[SquareMatrix[A]] {
      override def empty: SquareMatrix[A] =
        SquareMatrix((
          (Monoid[A].empty, Monoid[A].empty, Monoid[A].empty),
          (Monoid[A].empty, Monoid[A].empty, Monoid[A].empty),
          (Monoid[A].empty, Monoid[A].empty, Monoid[A].empty)
        ))
      override def combine(a: SquareMatrix[A], b: SquareMatrix[A]): SquareMatrix[A] = {
        val (
          (a11, a12, a13),
          (a21, a22, a23),
          (a31, a32, a33)
          ) = a.values
        val (
          (b11, b12, b13),
          (b21, b22, b23),
          (b31, b32, b33)
          ) = b.values
        SquareMatrix((
          (a11 |+| b11, a12 |+| b12, a13 |+| b13),
          (a21 |+| b21, a22 |+| b22, a23 |+| b23),
          (a31 |+| b31, a32 |+| b32, a33 |+| b33)
        ))
      }
    }
  }
}
