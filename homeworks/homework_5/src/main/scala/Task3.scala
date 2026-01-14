import cats._
import cats.implicits._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

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
      def combine(x: WordsCount, y: WordsCount): WordsCount = {
        val merged = (x.count ++ y.count).foldLeft(Map.empty[String, Int]) {
          case (acc, Count(w, c)) => acc.updated(w, acc.getOrElse(w, 0) + c)
        }
        WordsCount(merged.map { case (w, c) => Count(w, c) }.toSeq)
      }
    }

  }

  def countWords(lines: Vector[String]): WordsCount = {
    val fut = mapReduce[String, WordsCount](lines) { line =>
      val words = line.split(" ").filter(_.nonEmpty)
      val counts = words.foldLeft(Map.empty[String, Int]) { (acc, w) =>
        acc.updated(w, acc.getOrElse(w, 0) + 1)
      }
      WordsCount(counts.map { case (w, c) => Count(w, c) }.toSeq)
    }
    Await.result(fut, 5.seconds)
  }

}
