package ru.dru

import zio.CanFail.canFailAmbiguous1
import zio.{Clock, Duration, Exit, Fiber, Scope, ZIO, ZIOApp, ZIOAppArgs, ZIOAppDefault, durationInt}

import java.time.LocalDateTime
import scala.concurrent.TimeoutException

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
    def createTask(duration: Duration): ZIO[Any, Throwable, LocalDateTime] =
      ZIO.sleep(duration) *> Clock.localDateTime.mapError(_ => new Exception())
    val waterTask: ZIO[Any, Throwable, LocalDateTime] = createTask(waterBoilingTime)
    val eggsTask: ZIO[Any, Throwable, LocalDateTime] = createTask(eggsFiringTime)

    val saladTask: ZIO[Any, Throwable, LocalDateTime] =
      createTask(saladInfoTime.cucumberTime) *>
        createTask(saladInfoTime.tomatoTime) *>
        Clock.localDateTime.mapError(_ => new Exception())

    val teaTask: ZIO[Any, Throwable, LocalDateTime] = createTask(teaBrewingTime)

    for {
      waterFiber <- waterTask.fork
      eggsFiber <- eggsTask.fork
      saladFiber <- saladTask.fork
      waterTime <- waterFiber.join
      teaFiber <- teaTask.fork
      eggsTime <- eggsFiber.join
      saladTime <- saladFiber.join
      teaTime <- teaFiber.join
    } yield Map(
      "eggs" -> eggsTime,
      "water" -> waterTime,
      "saladWithSourCream" -> saladTime,
      "tea" -> teaTime
    )
  }



  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ZIO.succeed(println("Done"))

}
