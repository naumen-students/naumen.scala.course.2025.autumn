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
      def empty : WordsCount = WordsCount(Seq())
      def combine(x: WordsCount, y: WordsCount): WordsCount = {
        val unification = x.count ++ y.count
        val grouping = unification.groupBy(_.word)
        val resultSeq = grouping.map { case (word, counts) =>
          Count(word, counts.map(_.count).sum)
        }.toSeq
        WordsCount(resultSeq)
      }
    }
  }

  def countWords(lines: Vector[String]): WordsCount = {
    val future: Future[WordsCount] = mapReduce(lines)(lineToCount)
    Await.result(future, 5.seconds)
  }

  def lineToCount(line: String): WordsCount = {
    val words = line.split("\\s+").filter(_.nonEmpty)
    val counts = words.map(word => Count(word, 1))
    WordsCount(counts)
  }
}