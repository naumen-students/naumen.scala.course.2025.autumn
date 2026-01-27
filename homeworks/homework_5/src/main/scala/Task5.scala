object Task5 extends App {
  import Task4.MonadError

  sealed trait MyEither[+E, +A] {
    def isError: Boolean
  }

  final case class MyRight[+A](value: A) extends MyEither[Nothing, A] {
    override def isError: Boolean = false
  }

  final case class MyLeft[+E](error: E) extends MyEither[E, Nothing] {
    override def isError: Boolean = true
  }

  object MyEither {
    def apply[A](value: A): MyEither[Nothing, A] = MyRight(value)

    def error[E, A](error: E): MyEither[E, A] = MyLeft(error)

    def possibleError[A](f: => A): MyEither[Throwable, A] =
      try MyRight(f)
      catch { case t: Throwable => MyLeft(t) }

    implicit def myEitherMonad[E]: MonadError[MyEither, E] = new MonadError[MyEither, E] {
      override def pure[A](value: A): MyEither[E, A] = MyRight(value)

      override def flatMap[A, B](fa: MyEither[E, A])(f: A => MyEither[E, B]): MyEither[E, B] =
        fa match {
          case MyRight(a) => f(a)
          case MyLeft(e)  => MyLeft(e)
        }

      override def map[A, B](fa: MyEither[E, A])(f: A => B): MyEither[E, B] =
        fa match {
          case MyRight(a) => MyRight(f(a))
          case MyLeft(e)  => MyLeft(e)
        }

      override def raiseError[A](fa: MyEither[E, A])(error: => E): MyEither[E, A] =
        MyLeft(error)

      override def handleError[A](fa: MyEither[E, A])(handle: E => A): MyEither[E, A] =
        fa match {
          case MyLeft(e)  => MyRight(handle(e))
          case r @ MyRight(_) => r
        }
    }
  }

  object MyEitherSyntax {
    implicit class MyEitherOps[E, A](val either: MyEither[E, A]) extends AnyVal {
      def flatMap[B](f: A => MyEither[E, B]): MyEither[E, B] =
        MyEither.myEitherMonad[E].flatMap(either)(f)

      def map[B](f: A => B): MyEither[E, B] =
        MyEither.myEitherMonad[E].map(either)(f)

      def handleError(f: E => A): MyEither[E, A] =
        MyEither.myEitherMonad[E].handleError(either)(f)
    }
  }
}
