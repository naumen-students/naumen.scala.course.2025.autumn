import utils.ColorService.ColorService
import utils.PictureGenerationService.PictureGenerationService
import utils.Utils._
import zio.{IO, URIO, ZIO}

import java.awt.Color

object Exercises {

  def task1(r: Int, g: Int, b: Int): URIO[ColorService, Option[Color]] =
    ZIO.serviceWithZIO[ColorService](_.getColor(r, g, b)).either.map {
      case Right(color) => Some(color)
      case Left(_) => None
    }

  def task2(size: (Int, Int)): ZIO[PictureGenerationService, GenerationError, String] =
    ZIO.serviceWithZIO[PictureGenerationService](_.generatePicture(size))
      .map(p => p.lines
        .map(l => l.map(c => c.getRGB & 0xFFFFFF).mkString(" "))
        .mkString("\n"))

  def task3(size: (Int, Int)): ZIO[PictureGenerationService with ColorService, GenerationError, Picture] =
    for {
      colorServ <- ZIO.service[ColorService]
      pictureServ <- ZIO.service[PictureGenerationService]

      color <- colorServ.generateRandomColor()
        .mapError(_ => new GenerationError("Не удалось создать цвет"))

      picture <- pictureServ.generatePicture(size)
        .mapError(_ => new GenerationError("Ошибка генерации изображения"))

      filled <- pictureServ.fillPicture(picture, color)
        .mapError(_ => new GenerationError("Возникли проблемы при заливке изображения"))
    } yield filled

  def task4(size: (Int, Int)): IO[GenerationError, Picture] =
    task3(size)
      .provide(
        utils.ColorService.live,
        utils.PictureGenerationService.live
      )
}
