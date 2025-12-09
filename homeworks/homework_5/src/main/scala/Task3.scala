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
    private def toMap(wc: WordsCount): Map[String, Int] =
      wc.count
        .groupBy(_.word)
        .map { case (word, counts) =>
          word -> counts.map(_.count).sum
        }

    private def fromMap(m: Map[String, Int]): WordsCount =
      WordsCount(
        m.toSeq
          .sortBy(_._1)                 // стабильный порядок
          .map { case (w, c) => Count(w, c) }
      )

    implicit val monoid: Monoid[WordsCount] = new Monoid[WordsCount] {
      override def empty: WordsCount = WordsCount(Seq.empty)

      override def combine(a: WordsCount, b: WordsCount): WordsCount = {
        val am = toMap(a)
        val bm = toMap(b)

        val merged = (am.keySet ++ bm.keySet).iterator.map { k =>
          k -> (am.getOrElse(k, 0) + bm.getOrElse(k, 0))
        }.toMap

        fromMap(merged)
      }
    }
  }

  private def countsFromLine(line: String): WordsCount = {
    val counts =
      line
        .split("\\s+")
        .iterator
        .filter(_.nonEmpty)
        .map(word => Count(word, 1))
        .toSeq

    WordsCount(counts)
  }

  def countWords(lines: Vector[String]): WordsCount = {
    val future = mapReduce(lines)(countsFromLine)
    Await.result(future, 10.seconds)
  }
}
