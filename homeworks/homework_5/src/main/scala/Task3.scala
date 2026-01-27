import cats._
import cats.implicits._

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt

/*
  Задание №3
  Всё просто, нужно посчитать количество строк.
  Реализуйте функцию countWords, которая принимает список строк.
  Обязательно использовать функцию mapReduce.
 */
object Task3 extends App {
  def mapReduce[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    val numCores = Runtime.getRuntime.availableProcessors
    val groupSize = (1.0 * values.size / numCores).ceil.toInt
    values
      .grouped(groupSize)
      .toVector
      .traverse(group => Future(group.foldMap(func)))
      .map(_.combineAll)
  }

  case class Count(word: String, count: Int)

  case class WordsCount(count: Seq[Count])

  object WordsCount {
    implicit val monoid: Monoid[WordsCount] = new Monoid[WordsCount] {
      override def empty: WordsCount = WordsCount(Seq.empty)

      override def combine(a: WordsCount, b: WordsCount): WordsCount = {
        val grouped = (a.count ++ b.count)
          .groupBy(_.word)
          .map { case (word, counts) =>
            Count(word, counts.map(_.count).sum)
          }
          .toSeq
        WordsCount(grouped)
      }
    }
  }

  def countWords(lines: Vector[String]): WordsCount = {
    val futureResult = mapReduce(lines) { line =>
      val counts = line
        .split("\\s+")
        .filter(_.nonEmpty)
        .map(w => Count(w, 1))
      WordsCount(counts.toSeq)
    }
    Await.result(futureResult, 5.seconds)
  }
}
