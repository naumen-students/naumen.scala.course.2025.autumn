package ru.dru

import zio.{Duration, Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.time.LocalDateTime

case class SaladInfoTime(tomatoTime: Duration, cucumberTime: Duration)


object Breakfast extends ZIOAppDefault {

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
                    teaBrewingTime: Duration): ZIO[Any, Throwable, Map[String, LocalDateTime]] = {
    val waterTask = for {
      _ <- ZIO.sleep(waterBoilingTime)
    } yield "water" -> LocalDateTime.now()
    val eggsTask = for {
      _ <- ZIO.sleep(eggsFiringTime)
    } yield "eggs" -> LocalDateTime.now()
    val saladTask = for {
      _ <- ZIO.sleep(saladInfoTime.cucumberTime)
      _ <- ZIO.sleep(saladInfoTime.tomatoTime)
    } yield "saladWithSourCream" -> LocalDateTime.now()
    val teaTask = for {
      _ <- waterTask
      _ <- ZIO.sleep(teaBrewingTime)
    } yield "tea" -> LocalDateTime.now()

    for {
      waterFiber <- waterTask.fork
      eggsFiber <- eggsTask.fork
      saladFiber <- saladTask.fork
      teaFiber <- teaTask.fork

      waterResult <- waterFiber.join
      eggsResult <- eggsFiber.join
      saladResult <- saladFiber.join
      teaResult <- teaFiber.join
    } yield Map(eggsResult, waterResult, saladResult, teaResult)
  }



  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ZIO.succeed(println("Done"))

}
