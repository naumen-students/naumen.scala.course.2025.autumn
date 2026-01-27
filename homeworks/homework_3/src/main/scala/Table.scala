import scala.collection.mutable

class Table(width: Int, height: Int) {
  private val cells = mutable.ArrayBuffer.fill(width * height)(new EmptyCell())

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    if (ix < 0 || iy < 0 || ix >= width || iy >= height) None
    else {
      val i = ix + iy * width
      Some(cells(i))
    }
  }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    if (ix >= 0 && iy >= 0 && ix < width && iy < height) {
      val i = ix + iy * width
      cells(i) = cell
    }
  }
}
