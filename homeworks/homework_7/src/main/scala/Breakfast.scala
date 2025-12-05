package ru.dru

import zio.{Duration, ZIO, ZIOAppDefault}
import java.time.LocalDateTime

case class SaladInfoTime(tomatoTime: Duration, cucumberTime: Duration)

object Breakfast extends ZIOAppDefault {

  def makeBreakfast(
                     eggsFiringTime: Duration,
                     waterBoilingTime: Duration,
                     saladInfoTime: SaladInfoTime,
                     teaBrewingTime: Duration
                   ): ZIO[Any, Throwable, Map[String, LocalDateTime]] = {

    val waterTask = ZIO.sleep(waterBoilingTime).as("water" -> LocalDateTime.now())
    val eggsTask = ZIO.sleep(eggsFiringTime).as("eggs" -> LocalDateTime.now())
    val saladTask = (ZIO.sleep(saladInfoTime.cucumberTime) *>
      ZIO.sleep(saladInfoTime.tomatoTime)).as("saladWithSourCream" -> LocalDateTime.now())
    val teaTask = waterTask.flatMap { waterResult =>
      ZIO.sleep(teaBrewingTime).as("tea" -> LocalDateTime.now())
    }

    for {
      waterFiber <- waterTask.fork
      eggsFiber <- eggsTask.fork
      saladFiber <- saladTask.fork
      teaFiber <- teaTask.fork

      waterTime <- waterFiber.join
      eggsTime <- eggsFiber.join
      saladTime <- saladFiber.join
      teaTime <- teaFiber.join
    } yield Map(waterTime, eggsTime, saladTime, teaTime)
  }

  override def run: ZIO[Any, Nothing, Unit] = ZIO.succeed(println("Done"))
}