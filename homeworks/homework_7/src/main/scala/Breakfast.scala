package ru.dru

import zio._
import java.time.LocalDateTime

case class SaladInfoTime(tomatoTime: zio.Duration, cucumberTime: zio.Duration)

object Breakfast extends ZIOAppDefault {

  /**
   * Функция должна эмулировать приготовление завтрака. Продолжительные операции необходимо эмулировать через ZIO.sleep.
   * Правила приготовления следующие:
   *  1. Нобходимо вскипятить воду (время кипячения waterBoilingTime)
   *  2. Параллельно с этим нужно жарить яичницу eggsFiringTime
   *  3. Параллельно с этим готовим салат:
   *    * сначала режим огурцы
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
  def makeBreakfast(
                     eggsFiringTime: zio.Duration,
                     waterBoilingTime: zio.Duration,
                     saladInfoTime: SaladInfoTime,
                     teaBrewingTime: zio.Duration
                   ): ZIO[Any, Throwable, Map[String, LocalDateTime]] = {

    for {
      // Запоминаем время начала
      startTime <- Clock.currentDateTime.map(_.toLocalDateTime)

      // Запускаем ВСЕ задачи параллельно, но с правильными зависимостями
      result <- ZIO.scoped {
        for {
          // 1. Кипятим воду (запускаем сразу)
          waterFiber <- ZIO.sleep(waterBoilingTime)
            .map(_ => "water" -> startTime.plusSeconds(waterBoilingTime.toSeconds))
            .fork

          // 2. Жарим яичницу (запускаем сразу, параллельно с водой)
          eggsFiber <- ZIO.sleep(eggsFiringTime)
            .map(_ => "eggs" -> startTime.plusSeconds(eggsFiringTime.toSeconds))
            .fork

          // 3. Готовим салат: последовательные операции
          saladFiber <- (for {
            // Сначала режем огурцы
            _ <- ZIO.sleep(saladInfoTime.cucumberTime)
            // Потом режем помидоры
            _ <- ZIO.sleep(saladInfoTime.tomatoTime)
            // Потом добавляем сметану (мгновенно по условию)
          } yield "saladWithSourCream" -> startTime.plusSeconds(
            saladInfoTime.cucumberTime.toSeconds + saladInfoTime.tomatoTime.toSeconds
          )).fork

          // 4. Чай: ЖДЕМ пока закипит вода, потом завариваем
          teaFiber <- (for {
            // Ждем завершения кипячения воды
            _ <- waterFiber.join
            // Завариваем чай
            _ <- ZIO.sleep(teaBrewingTime)
          } yield "tea" -> startTime.plusSeconds(
            waterBoilingTime.toSeconds + teaBrewingTime.toSeconds
          )).fork

          // Ждем завершения ВСЕХ задач
          waterResult <- waterFiber.join
          eggsResult <- eggsFiber.join
          saladResult <- saladFiber.join
          teaResult <- teaFiber.join

        } yield Map(waterResult, eggsResult, saladResult, teaResult)
      }

    } yield result
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] =
    ZIO.succeed(println("Done"))
}
