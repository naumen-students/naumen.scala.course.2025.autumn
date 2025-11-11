import scala.util.{Failure, Success, Try}

object Task5 extends App {
  import Task4.MonadError

  sealed trait MyEither[+E, +A] {
    def isError: Boolean
  }

  case class MyRight[+A](value: A) extends MyEither[Nothing, A] {
    override def isError: Boolean = false
  }

  case class MyLeft[+E](error: E) extends MyEither[E, Nothing] {
    override def isError: Boolean = true
  }

  object MyEither {
    def apply[A](value: A): MyEither[Nothing, A] = MyRight(value)

    def error[E, A](error: E): MyEither[E, A] = MyLeft(error)

    def possibleError[A](f: => A): MyEither[Throwable, A] =
      try {
        MyRight(f)
      } catch {
        case e: Throwable => MyLeft(e)
      }

    implicit def myEitherMonad[E]: MonadError[MyEither, E] = new MonadError[MyEither, E] {
      override def pure[A](value: A): MyEither[E, A] = MyRight(value)

      override def flatMap[A, B](fa: MyEither[E, A])(f: A => MyEither[E, B]): MyEither[E, B] =
        fa match {
          case MyLeft(e) => MyLeft(e)
          case MyRight(a) => f(a)
        }

      override def map[A, B](fa: MyEither[E, A])(f: A => B): MyEither[E, B] =
        flatMap(fa)(a => pure(f(a)))

      override def raiseError[A](fa: MyEither[E, A])(error: => E): MyEither[E, A] =
        MyLeft(error)

      override def handleError[A](fa: MyEither[E, A])(handle: E => A): MyEither[E, A] =
        fa match {
          case MyLeft(e) => MyRight(handle(e))
          case MyRight(a) => MyRight(a)
        }
    }
  }

  object MyEitherSyntax {
    implicit class MyEitherOps[E, A](val either: MyEither[E, A]) {
      def flatMap[B](f: A => MyEither[E, B]): MyEither[E, B] =
        MyEither.myEitherMonad[E].flatMap(either)(f)

      def map[B](f: A => B): MyEither[E, B] =
        MyEither.myEitherMonad[E].map(either)(f)

      def handleError(f: E => A): MyEither[E, A] =
        MyEither.myEitherMonad[E].handleError(either)(f)
    }
  }
}
