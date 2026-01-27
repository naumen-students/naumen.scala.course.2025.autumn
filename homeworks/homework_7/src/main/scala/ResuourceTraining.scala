package ru.dru

import zio.{IO, Scope, ZIO, ZIOAppArgs, ZIOAppDefault}
import java.io.{BufferedReader, BufferedWriter, FileReader, FileWriter}

object ResuourceTraining extends ZIOAppDefault {

  def readData(filePath: String): IO[Throwable, String] = {
    ZIO.acquireReleaseWith(
      ZIO.attempt(new BufferedReader(new FileReader(filePath)))
    )(reader => 
      ZIO.succeed(reader.close())
    ) { reader =>
      ZIO.attempt {
        val content = new StringBuilder
        var line: String = reader.readLine()
        while (line != null) {
          content.append(line)
          line = reader.readLine()
        }
        content.toString
      }
    }
  }

  def writeData(filePath: String, data: String): ZIO[Any, Nothing, Unit] = {
    ZIO.acquireReleaseWith(
      ZIO.attempt(new BufferedWriter(new FileWriter(filePath, false)))
    )(writer => 
      ZIO.succeed(writer.close())
    ) { writer =>
      ZIO.attempt {
        writer.write(data)
        writer.flush()
      }
    }.catchAll(_ => ZIO.unit) // Преобразуем любые ошибки в Nothing
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ZIO.succeed("Done")
}
