package ru.dru

import zio.CanFail.canFailAmbiguous1
import zio.{Duration, Exit, Task, Scope, ZIO, ZIOApp, ZIOAppArgs, ZIOAppDefault, durationInt}

import java.time.LocalDateTime
import scala.concurrent.TimeoutException

case class SaladInfoTime(tomatoTime: Duration, cucumberTime: Duration)



object Breakfast extends ZIOAppDefault {

  private def boilWater(waterBoilingTime: Duration): ZIO[Any, Nothing, LocalDateTime] =
    ZIO.sleep(waterBoilingTime) *> ZIO.succeed(LocalDateTime.now())

  private def fryEggs(eggsFiringTime: Duration): ZIO[Any, Nothing, LocalDateTime] =
    ZIO.sleep(eggsFiringTime) *> ZIO.succeed(LocalDateTime.now())

  private def cutSalad(saladInfoTime: SaladInfoTime): ZIO[Any, Nothing, LocalDateTime] =
    for {
      _ <- ZIO.sleep(saladInfoTime.cucumberTime)
      _ <- ZIO.sleep(saladInfoTime.tomatoTime)
      saladEndTime <- ZIO.succeed(LocalDateTime.now())
    } yield saladEndTime

  private def brewTea(teaBrewingTime: Duration): ZIO[Any, Nothing, LocalDateTime] =
    ZIO.sleep(teaBrewingTime) *> ZIO.succeed(LocalDateTime.now())

  /**
   * Функция должна эмулировать приготовление завтрака. Продолжительные операции необходимо эмулировать через ZIO.sleep.
   * Правила приготовления следующие:
   *  1. Нобходимо вскипятить воду (время кипячения waterBoilingTime)
   *  2. Параллельно с этим нужно жарить яичницу eggsFiringTime
   *  3. Параллельно с этим готовим салат:
   *    * сначала режим  огурцы
   *    * после этого режим помидоры
   *    * после этого добавляем в салат сметану
   *  4. После того, как закипит вода необходимо заварить чай, время заваривания чая teaBrewingTime
   *  5. После того, как всё готово, можно завтракать
   *
   * @param eggsFiringTime время жарки яичницы
   * @param waterBoilingTime время кипячения воды
   * @param saladInfoTime информация о времени для приготовления салата
   * @param teaBrewingTime время заваривания чая
   * @return Мапу с информацией о том, когда завершился очередной этап (eggs, water, saladWithSourCream, tea)
   */
  def makeBreakfast(eggsFiringTime: Duration,
                    waterBoilingTime: Duration,
                    saladInfoTime: SaladInfoTime,
                    teaBrewingTime: Duration): ZIO[Any, Throwable, Map[String, LocalDateTime]] =
    for {
      waterTask <- boilWater(waterBoilingTime).fork
      eggsTask <- fryEggs(eggsFiringTime).fork
      saladTask <- cutSalad(saladInfoTime).fork

      waterEndTime <- waterTask.join

      teaTask <- brewTea(teaBrewingTime).fork

      eggsEndTime <- eggsTask.join
      saladEndTime <- saladTask.join
      teaEndTime <- teaTask.join

    } yield Map(
      "eggs" -> eggsEndTime,
      "water" -> waterEndTime,
      "saladWithSourCream" -> saladEndTime,
      "tea" -> teaEndTime
    )



  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ZIO.succeed(println("Done"))

}
