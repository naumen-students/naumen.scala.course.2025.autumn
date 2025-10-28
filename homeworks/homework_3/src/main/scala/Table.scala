package homework_3

import scala.collection.mutable

class Table(val width: Int, val height: Int) {
  private val cells: mutable.Buffer[Cell] =
    mutable.Buffer.fill(width * height)(new EmptyCell())

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    if (ix < 0 || iy < 0 || ix >= width || iy >= height) None
    else Some(cells(ix + iy * width))
  }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    if (ix >= 0 && iy >= 0 && ix < width && iy < height)
      cells(ix + iy * width) = cell
  }
}
