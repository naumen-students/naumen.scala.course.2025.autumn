import cats._
import cats.implicits._

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

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
      def empty: WordsCount = WordsCount(Seq.empty)

      def combine(a: WordsCount, b: WordsCount): WordsCount = {
        val mapA: Map[String, Int] = a.count.map(c => c.word -> c.count).toMap
        val mapB: Map[String, Int] = b.count.map(c => c.word -> c.count).toMap
        val merged: Map[String, Int] = (mapA.keySet ++ mapB.keySet).map { k =>
          k -> (mapA.getOrElse(k, 0) + mapB.getOrElse(k, 0))
        }.toMap
        WordsCount(merged.toSeq.map { case (w, c) => Count(w, c) })
      }
    }
  }

  def countWords(lines: Vector[String]): WordsCount = {
    val fut = mapReduce(lines) { line =>
      val wordsArr = line.split("\\s+").filter(_.nonEmpty)
      val grouped: Map[String, Int] = wordsArr
        .groupBy(identity)
        .map { case (word, arr) => word -> arr.length }

      WordsCount(grouped.toSeq.map { case (w, c) => Count(w, c) })
    }(WordsCount.monoid)

    import scala.concurrent.duration._
    import scala.concurrent.Await
    Await.result(fut, Duration.Inf)
  }
}
