import scala.collection.mutable

class Table(width: Int, height: Int) {

  private val cells: mutable.ArrayBuffer[Cell] =
    mutable.ArrayBuffer.fill(width * height)(new EmptyCell)

  private def getIndex(ix: Int, iy: Int): Int = {
    ix + iy * width
  }

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    if (ix >= width || ix < 0 || iy >= height || iy < 0)
      return None
    else
      return Some(cells(getIndex(ix, iy)))
  }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    if (ix >= 0 && iy >= 0 && ix < width && iy < height)
      cells(getIndex(ix, iy)) = cell
  }

  def getStringValue(ix: Int, iy: Int, visited: Set[(Int, Int)]): String = {
    if (ix >= width || ix < 0 || iy >= height || iy < 0)
      return "outOfRange"
    if (visited.contains((ix, iy)))
      return "cyclic"

    cells(getIndex(ix, iy)) match {
      case ref: ReferenceCell =>
        return ref.resolveReference(visited + ((ix, iy)))
      case cell =>
        return cell.toString
    }
  }
}