import scala.collection.mutable

class Table(width: Int, height: Int) {
  val cells: mutable.ArrayBuffer[Cell] =
    mutable.ArrayBuffer.fill[Cell](width * height)(new EmptyCell())

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    cells.lift(ix + iy * width)
  }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    if (ix < 0 || ix >= width || iy < 0 || iy >= height) return
    val i = ix + iy * width
    cells(i) = cell
  }
}
