import scala.util.{Failure, Success, Try}

object Task5 extends App {

  import Task4.MonadError

  sealed trait MyEither[+E, +A] {
    def isError: Boolean
  }

  private case class RightValue[+A](value: A) extends MyEither[Nothing, A] {
    def isError: Boolean = false
  }

  private case class LeftValue[+E](error: E) extends MyEither[E, Nothing] {
    def isError: Boolean = true
  }

  object MyEither {
    def apply[A](value: A): MyEither[Nothing, A] = RightValue(value)

    def error[E, A](error: E): MyEither[E, A] = LeftValue(error)

    def possibleError[A](f: => A): MyEither[Throwable, A] = {
      Try(f) match {
        case Success(value) => MyEither(value)
        case Failure(exception) => MyEither.error(exception)
      }
    }

    implicit def myEitherMonad[E]: MonadError[MyEither, E] = new MonadError[MyEither, E] {
      def pure[A](value: A): MyEither[E, A] = MyEither(value)

      def flatMap[A, B](fa: MyEither[E, A])(f: A => MyEither[E, B]): MyEither[E, B] = {
        fa match {
          case RightValue(a) => f(a)
          case LeftValue(e) => MyEither.error(e)
        }
      }

      def raiseError[A](fa: MyEither[E, A])(error: => E): MyEither[E, A] = MyEither.error(error)

      def handleError[A](fa: MyEither[E, A])(handle: E => A): MyEither[E, A] = {
        fa match {
          case RightValue(a) => MyEither(a)
          case LeftValue(e) => MyEither(handle(e))
        }
      }
    }
  }

  object MyEitherSyntax {
    implicit class MyEitherOps[E, A](val either: MyEither[E, A]) {
      def flatMap[B](f: A => MyEither[E, B]): MyEither[E, B] =
        MyEither.myEitherMonad[E].flatMap(either)(f)

      def map[B](f: A => B): MyEither[E, B] = MyEither.myEitherMonad.map(either)(f)

      def handleError(f: E => A): MyEither[E, A] =
        MyEither.myEitherMonad.handleError(either)(f)
    }
  }
}
