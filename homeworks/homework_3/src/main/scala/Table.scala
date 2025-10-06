class Table(val width: Int, val height: Int) {
  private val cells: Array[Cell] = Array.fill(width * height)(new EmptyCell)

  private def inRange(ix: Int, iy: Int): Boolean =
    ix >= 0 && iy >= 0 && ix < width && iy < height

  private def index(ix: Int, iy: Int): Int = ix + iy * width

  def getCell(ix: Int, iy: Int): Option[Cell] =
    if (inRange(ix, iy)) Some(cells(index(ix, iy))) else None

  def setCell(ix: Int, iy: Int, cell: Cell): Unit =
    if (inRange(ix, iy)) cells(index(ix, iy)) = cell

  def resolveToString(ix: Int, iy: Int, visited: Set[ReferenceCell]): String = {
    if (!inRange(ix, iy)) "outOfRange"
    else {
      cells(index(ix, iy)) match {
        case r: ReferenceCell =>
          if (visited.contains(r)) "cyclic"
          else r.table.resolveToString(r.ix, r.iy, visited + r)
        case c => c.toString
      }
    }
  }
}
