import scala.collection.mutable

class Table(width: Int, length: Int) {

  private val cells: mutable.ArrayBuffer[Cell] =
    mutable.ArrayBuffer.fill(width * length)(EmptyCell())

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    if (ix < 0 || ix >= width || iy < 0 || iy >= length) None
    else Some(cells(ix + iy * width))
  }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    if (ix >= 0 && ix < width && iy >= 0 && iy < length) {
      cells(ix + iy * width) = cell
    }
  }

  def resolveReference(ref: ReferenceCell, visited: mutable.Set[ReferenceCell]): String = {
    if (visited.contains(ref)) {
      return "cyclic"
    }

    visited += ref

    getCell(ref.ix, ref.iy) match {
      case None =>
        visited -= ref
        "outOfRange"

      case Some(targetCell) => targetCell match {
        case refCell: ReferenceCell =>
          val result = resolveReference(refCell, visited)
          visited -= ref
          result

        case otherCell =>
          visited -= ref
          otherCell.toString
      }
    }
  }
}
