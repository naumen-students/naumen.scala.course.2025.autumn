import cats._
import cats.implicits._

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt

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
        val merged = (a.count ++ b.count)
          .groupBy(_.word)
          .view
          .mapValues(_.map(_.count).sum)
          .toSeq
          .map { case (w, c) => Count(w, c) }
        WordsCount(merged)
      }
    }
  }

  def countWords(lines: Vector[String]): WordsCount = {
    val resultFuture = mapReduce(lines) { line =>
      val counts = line
        .split("\\s+")
        .filter(_.nonEmpty)
        .groupBy(identity)
        .view
        .mapValues(_.length)
        .toSeq
        .map { case (word, cnt) => Count(word, cnt) }
      WordsCount(counts)
    }
}
