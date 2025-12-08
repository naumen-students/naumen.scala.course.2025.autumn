package ru.dru

import zio.{IO, Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.io.{BufferedReader, BufferedWriter, FileReader, FileWriter}

object ResuourceTraining extends ZIOAppDefault {

  def readData(filePath: String): IO[Throwable, String] = {
    ZIO.acquireReleaseWith(
      // Acquire: открываем BufferedReader
      ZIO.attempt(new BufferedReader(new FileReader(filePath)))
    )(
      // Release: гарантированно закрываем reader
      reader => ZIO.succeed(reader.close()).ignore
    ) { reader =>
      // Use: читаем все содержимое файла
      ZIO.attempt {
        val sb = new StringBuilder
        var line: String = null

        while ({ line = reader.readLine(); line != null }) {
          if (sb.nonEmpty) sb.append(System.lineSeparator())
          sb.append(line)
        }

        sb.toString()
      }
    }
  }

  def writeData(filePath: String, data: String): ZIO[Any, Nothing, Unit] = {
    ZIO.acquireReleaseWith(
      // Acquire: открываем BufferedWriter
      ZIO.attempt(new BufferedWriter(new FileWriter(filePath, false)))
    )(
      // Release: гарантированно закрываем writer
      writer => ZIO.succeed(writer.close()).ignore
    ) { writer =>
      // Use: записываем данные
      ZIO.attempt(writer.write(data)).unit
    }.catchAll(_ => ZIO.unit) // Игнорируем все ошибки
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] =
    ZIO.succeed("Done")
}
