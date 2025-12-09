package ru.dru

import zio.{Duration, Scope, ZIO, ZIOApp, ZIOAppArgs, ZIOAppDefault}
import java.time.LocalDateTime

case class SaladInfoTime(tomatoTime: Duration, cucumberTime: Duration)

object Breakfast extends ZIOAppDefault {

  def makeBreakfast(
                     eggsFiringTime: Duration,
                     waterBoilingTime: Duration,
                     saladInfoTime: SaladInfoTime,
                     teaBrewingTime: Duration
                   ): ZIO[Any, Throwable, Map[String, LocalDateTime]] = {

    def now = ZIO.succeed(LocalDateTime.now())

    val eggs = for {
      _    <- ZIO.sleep(eggsFiringTime)
      time <- now
    } yield ("eggs", time)

    val water = for {
      _    <- ZIO.sleep(waterBoilingTime)
      time <- now
    } yield ("water", time)

    val salad = for {
      _    <- ZIO.sleep(saladInfoTime.cucumberTime)
      _    <- ZIO.sleep(saladInfoTime.tomatoTime)
      time <- now
    } yield ("saladWithSourCream", time)

    for {
      fiberEggs  <- eggs.fork
      fiberWater <- water.fork
      fiberSalad <- salad.fork

      eggsDone  <- fiberEggs.join
      waterDone <- fiberWater.join
      saladDone <- fiberSalad.join

      tea <- (for {
        _    <- ZIO.sleep(teaBrewingTime)
        time <- now
      } yield ("tea", time)).delay(waterBoilingTime)

    } yield Map(
      eggsDone._1  -> eggsDone._2,
      waterDone._1 -> waterDone._2,
      saladDone._1 -> saladDone._2,
      tea._1       -> tea._2
    )
  }

  override def run = ZIO.succeed(println("Done"))
}
