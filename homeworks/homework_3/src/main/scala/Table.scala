package homework3

class Table(val width: Int, val height: Int) {

  private val emptyCell = new EmptyCell

  private val cells: Array[Cell] =
    Array.fill(width * height)(emptyCell)

  private def index(ix: Int, iy: Int): Int =
    ix + iy * width

  private[homework3] def inBounds(ix: Int, iy: Int): Boolean =
    ix >= 0 && iy >= 0 && ix < width && iy < height

  def getCell(ix: Int, iy: Int): Option[Cell] =
    if (inBounds(ix, iy)) Some(cells(index(ix, iy))) else None

  private[homework3] def getCellSafe(ix: Int, iy: Int): Option[Cell] =
    getCell(ix, iy)

  def setCell(ix: Int, iy: Int, cell: Cell): Unit =
    if (inBounds(ix, iy)) cells(index(ix, iy)) = cell
}
