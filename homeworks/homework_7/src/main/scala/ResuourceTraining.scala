package ru.dru

import zio.{IO, Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.io.{BufferedReader, BufferedWriter, FileReader, FileWriter}
import java.util.stream.Collectors.joining


/**
 * Необходимо реализовать функции readData и writeData, записывающие и читающие данные в/из файла соответственно.
 * В реализации следует применять безопасное использование ресурсов ZIO.acquireReleaseWith
 */


object ResourceTraining extends ZIOAppDefault {

  def readData(filePath: String): IO[Throwable, String] =
    ZIO.acquireReleaseWith(
      ZIO.attemptBlocking(new BufferedReader(new FileReader(filePath)))
    )( reader => ZIO.attemptBlocking(reader.close()).orDie
    )( reader =>
      ZIO.attemptBlocking(
        reader.lines().collect(joining(System.lineSeparator()))
      )
    )

  def writeData(filePath: String, data: String): ZIO[Any, Nothing, Unit] =
    ZIO.acquireReleaseWith[Any, Nothing, BufferedWriter](
      ZIO.attemptBlocking(new BufferedWriter(new FileWriter(filePath))).orDie
    )(
      (writer: BufferedWriter) => ZIO.attemptBlocking(writer.close()).orDie
    ) { (writer: BufferedWriter) =>
      ZIO.attemptBlocking(writer.write(data)).orDie
    }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] =
    for {
      _ <- writeData("hello.txt", "Hello Abobus\nHey")
      content <- readData("hello.txt")
      _ <- zio.Console.printLine(s"Прочитал: \n$content")
    } yield "Done"
}