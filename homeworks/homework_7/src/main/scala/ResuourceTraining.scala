package ru.dru

import zio.{IO, Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.io.{BufferedReader, BufferedWriter, FileReader, FileWriter}


/**
 * Необходимо реализовать функции readData и writeData, записывающие и читающие данные в/из файла соответственно.
 * В реализации следует применять безопасное использование ресурсов ZIO.acquireReleaseWith
 */


object ResuourceTraining extends ZIOAppDefault {

  def readData(filePath: String): IO[Throwable, String] =  {
    val acquire: IO[Throwable, BufferedReader] =
      ZIO.attempt(new BufferedReader(new FileReader(filePath)))

    val release: BufferedReader => ZIO[Any, Nothing, Unit] =
      reader => ZIO.attempt(reader.close()).ignore

    val use: BufferedReader => IO[Throwable, String] =
      reader => ZIO.attempt {
        val content = new StringBuilder
        var line: String = null
        while ({line = reader.readLine(); line != null}) {
          content.append(line)
        }
        content.toString()
      }

    ZIO.acquireReleaseWith(acquire)(release)(use)
  }

  def writeData(filePath: String, data: String): ZIO[Any, Nothing, Unit] = {
    val acquire: ZIO[Any, Throwable, BufferedWriter] =
      ZIO.attempt(new BufferedWriter(new FileWriter(filePath)))

    val release: BufferedWriter => ZIO[Any, Nothing, Unit] =
      writer => ZIO.attempt(writer.close()).ignore

    val use: BufferedWriter => ZIO[Any, Nothing, Unit] =
      writer => ZIO.attempt {
        writer.write(data)
        writer.flush()
      }.catchAll(_ => ZIO.unit)

    ZIO.acquireReleaseWith(acquire)(release)(use).catchAll(_ => ZIO.unit)
  }


  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ZIO.succeed("Done")
}