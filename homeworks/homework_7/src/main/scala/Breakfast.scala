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
                    teaBrewingTime: Duration): ZIO[Any, Throwable, Map[String, LocalDateTime]] =
    for {
      start <- ZIO.succeed(LocalDateTime.now())
      water = ZIO.sleep(waterBoilingTime).as("water" -> start.plus(waterBoilingTime))
      eggs = ZIO.sleep(eggsFiringTime).as("eggs" -> start.plus(eggsFiringTime))
      salatTime = saladInfoTime.cucumberTime.plus(saladInfoTime.tomatoTime)
      salad = ZIO.sleep(salatTime).as("saladWithSourCream" -> start.plus(salatTime))
      breakfastWithoutTea <- ZIO.forkAll(List(water, eggs, salad))
      waterResult <- water
      tea <- ZIO.sleep(teaBrewingTime).as("tea" -> waterResult._2.plus(teaBrewingTime))
      result <- breakfastWithoutTea.join
    } yield (result :+ tea).toMap


  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ZIO.succeed(println("Done"))

}
