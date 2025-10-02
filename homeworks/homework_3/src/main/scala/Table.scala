import scala.collection.mutable

class Table(val width: Int, val height: Int) {
  require(width > 0 && height > 0, "Высота и длинна не могут быть 0")

  private val cells: mutable.ArrayBuffer[Cell] =
    mutable.ArrayBuffer.fill(width * height)(new EmptyCell)

  private def index(ix: Int, iy: Int): Int = ix + iy * width

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    if (ix < 0 || ix >= width || iy < 0 || iy >= height) None
    else Some(cells(index(ix, iy)))
  }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    if (ix >= 0 && ix < width && iy >= 0 && iy < height) {
      cells(index(ix, iy)) = cell
    }
  }

  def resolveReference(ref: ReferenceCell, visited: Set[ReferenceCell]): String = {
    if (visited.contains(ref)) {
      return "cyclic"
    }

    getCell(ref.ix, ref.iy) match {
      case Some(cell: ReferenceCell) =>
        resolveReference(cell, visited + ref)
      case Some(otherCell) =>
        otherCell.toString
    }
  }
}