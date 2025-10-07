import scala.collection.mutable.ArrayBuffer

// таблица
class Table(val width: Int, val height: Int) {
  private val cells: ArrayBuffer[Cell] = ArrayBuffer.fill(width * height)(new EmptyCell)

  private def getIndex(ix: Int, iy: Int): Int = ix + iy * width

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    if (ix >= 0 && ix < width && iy >= 0 && iy < height) {
      Some(cells(getIndex(ix, iy)))
    } else {
      None
    }
  }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    if (ix >= 0 && ix < width && iy >= 0 && iy < height) {
      cells(getIndex(ix, iy)) = cell
      // при наличии ссылки указать координаты, либо пропустить этап
      cell match {
        case refCell: ReferenceCell => refCell.setSelfCoordinates(ix, iy)
        case _ =>
      }
    }
  }
}