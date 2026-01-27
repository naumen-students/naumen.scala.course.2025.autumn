import cats._
import cats.implicits._

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object Task3 extends App {
  def mapReduce[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    val cores     = math.max(1, Runtime.getRuntime.availableProcessors)
    val groupSize = math.max(1, math.ceil(values.size.toDouble / cores).toInt)

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
        val mergedMap: Map[String, Int] =
          (a.count.iterator ++ b.count.iterator)
            .foldLeft(Map.empty[String, Int]) { (acc, c) =>
              val n = acc.getOrElse(c.word, 0) + c.count
              acc.updated(c.word, n)
            }

        WordsCount(mergedMap.iterator.map { case (w, n) => Count(w, n) }.toSeq)
      }
    }
  }

  def countWords(lines: Vector[String]): WordsCount = {
    val fut = mapReduce[String, WordsCount](lines) { line =>
      val localMap: Map[String, Int] =
        line.split("\\s+").iterator.filter(_.nonEmpty)
          .foldLeft(Map.empty[String, Int]) { (acc, w) =>
            acc.updated(w, acc.getOrElse(w, 0) + 1)
          }

      WordsCount(localMap.iterator.map { case (w, n) => Count(w, n) }.toSeq)
    }
    Await.result(fut, 10.seconds)
  }
}
