package ru.dru

import zio.{IO, ZIO, ZIOAppDefault}
import java.io.{BufferedReader, BufferedWriter, FileReader, FileWriter}

object ResuourceTraining extends ZIOAppDefault {

  def readData(filePath: String): IO[Throwable, String] = {
    ZIO.acquireReleaseWith(
      ZIO.attempt(new BufferedReader(new FileReader(filePath)))
    )(
      reader => ZIO.attempt(reader.close()).catchAll(_ => ZIO.unit)
    ) { reader => ZIO.attempt {
      reader.readLine()
    }
    }
  }

  def writeData(filePath: String, data: String): ZIO[Any, Nothing, Unit] = {
    ZIO.acquireReleaseWith(
      ZIO.attempt(new BufferedWriter(new FileWriter(filePath)))
    )(
      writer => ZIO.attempt(writer.close()).catchAll(_ => ZIO.unit)
    ) { writer =>
      ZIO.attempt(writer.write(data)).catchAll(_ => ZIO.unit)
    }.catchAll(_ => ZIO.unit)
  }

  override def run: ZIO[Any, Nothing, Unit] = ZIO.succeed(println("Done"))
}