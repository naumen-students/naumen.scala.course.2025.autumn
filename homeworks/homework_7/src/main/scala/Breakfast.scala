package ru.dru

import zio.{Duration, ZIO, Clock, durationInt}
import java.time.LocalDateTime

// Исправляем case class согласно тестам
case class SaladInfoTime(cuttingTime: Duration, mixingTime: Duration)

object Breakfast extends ZIOAppDefault {

  def makeBreakfast(eggsFiringTime: Duration,
                    waterBoilingTime: Duration,
                    saladInfoTime: SaladInfoTime,
                    teaBrewingTime: Duration): ZIO[Any, Throwable, Map[String, LocalDateTime]] = {
    
    for {
      startTime <- Clock.currentDateTime
      
      // Параллельно запускаем яичницу, кипячение воды и салат
      eggsFiber <- ZIO.sleep(eggsFiringTime).fork
      waterFiber <- ZIO.sleep(waterBoilingTime).fork
      saladFiber <- makeSalad(saladInfoTime).fork
      
      // Ждем когда вода вскипит, чтобы начать заваривать чай
      _ <- waterFiber.join
      teaFiber <- ZIO.sleep(teaBrewingTime).fork
      
      // Ждем завершения всех процессов
      _ <- eggsFiber.join
      _ <- saladFiber.join  
      _ <- teaFiber.join
      
      // Рассчитываем время завершения для каждого компонента
      eggsTime = startTime.plusSeconds(eggsFiringTime.getSeconds)
      waterTime = startTime.plusSeconds(waterBoilingTime.getSeconds)
      saladTime = startTime.plusSeconds(saladInfoTime.cuttingTime.getSeconds + saladInfoTime.mixingTime.getSeconds)
      teaTime = startTime.plusSeconds(waterBoilingTime.getSeconds + teaBrewingTime.getSeconds)
      
    } yield Map(
      "eggs" -> eggsTime,
      "water" -> waterTime,
      "saladWithSourCream" -> saladTime,
      "tea" -> teaTime
    )
  }

  private def makeSalad(saladInfoTime: SaladInfoTime): ZIO[Any, Nothing, Unit] = {
    ZIO.sleep(saladInfoTime.cuttingTime) *> ZIO.sleep(saladInfoTime.mixingTime)
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ZIO.succeed(println("Done"))
}
