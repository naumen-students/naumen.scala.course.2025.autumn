import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

object Task4 extends App {

  trait MonadError[F[_, _], E] {
    def pure[A](value: A): F[E, A]
    def flatMap[A, B](fa: F[E, A])(f: A => F[E, B]): F[E, B]

    def map[A, B](fa: F[E, A])(f: A => B): F[E, B] =
      flatMap(fa)(a => pure(f(a)))

    def raiseError[A](fa: F[E, A])(error: => E): F[E, A]
    def handleError[A](fa: F[E, A])(handle: E => A): F[E, A]
  }

  case class EIO[+E, +A](value: Either[E, A])
  object EIO {
    def apply[A](value: A): EIO[Nothing, A] = EIO[Nothing, A](Right(value))

    def error[E, A](error: E): EIO[E, A] = EIO[E, A](Left(error))

    def possibleError[A](f: => A): EIO[Throwable, A] =
      try {
        EIO(Right(f))
      } catch {
        case e: Throwable => EIO(Left(e))
      }

    implicit def monad[E]: MonadError[EIO, E] = new MonadError[EIO, E] {
      override def pure[A](value: A): EIO[E, A] = EIO(Right(value))

      override def flatMap[A, B](fa: EIO[E, A])(f: A => EIO[E, B]): EIO[E, B] =
        fa.value match {
          case Left(err) => EIO(Left(err))
          case Right(a)  => f(a)
        }

      override def raiseError[A](fa: EIO[E, A])(error: => E): EIO[E, A] =
        EIO(Left(error))

      override def handleError[A](fa: EIO[E, A])(handle: E => A): EIO[E, A] =
        fa.value match {
          case Left(err)  => EIO(Right(handle(err)))
          case Right(a)   => EIO(Right(a))
        }
    }
  }

  object EIOSyntax {
    implicit class EIOOps[E, A](val eio: EIO[E, A]) {
      def flatMap[B](f: A => EIO[E, B]): EIO[E, B] =
        EIO.monad[E].flatMap(eio)(f)

      def map[B](f: A => B): EIO[E, B] =
        EIO.monad[E].map(eio)(f)

      def handleError(f: E => A): EIO[E, A] =
        EIO.monad[E].handleError(eio)(f)
    }
  }
}
