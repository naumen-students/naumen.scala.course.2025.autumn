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

      override def combine(x: WordsCount, y: WordsCount): WordsCount = {
        val allCounts: Seq[(String, Int)] = (x.count ++ y.count).map(c => c.word -> c.count)

        val initialMap: Map[String, Int] = Map.empty
        val combinedMap: Map[String, Int] = allCounts.foldLeft(initialMap) {
          case (acc, (word, count)) =>
            acc + (word -> (acc.getOrElse(word, 0) + count))
        }

        WordsCount(combinedMap.map { case (word, count) => Count(word, count) }.toSeq)
      }
    }
  }

  def countWords(lines: Vector[String]): WordsCount = {
    val lineToWordsCount: String => WordsCount = line => {
      val words: Array[String] = line.split("\\s+").filter(_.nonEmpty)

      val countsMap: Map[String, Int] = words.foldLeft(Map.empty[String, Int]) {
        case (acc, word) =>
          acc + (word -> (acc.getOrElse(word, 0) + 1))
      }

      val countsSeq = countsMap.map { case (word, count) => Count(word, count) }.toSeq
      WordsCount(countsSeq)
    }

    val futureResult: Future[WordsCount] = mapReduce(lines)(lineToWordsCount)

    Await.result(futureResult, 5.seconds)
  }
}