import scala.collection.mutable

class Table(rowCount: Int, columnCount: Int) {
  private val table: mutable.ArraySeq[Cell] = mutable.ArraySeq.fill(rowCount * columnCount)(new EmptyCell)

  private def getIndex(ix: Int, iy: Int): Int = ix + iy * columnCount

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    if (isCellInTable(ix, iy))
      Some(table(getIndex(ix, iy)))
    else
      None
  }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    if (isCellInTable(ix, iy))
    table(getIndex(ix, iy)) = cell
  }

  def isCellInTable(ix: Int, iy: Int): Boolean =
    ix >= 0 && ix < columnCount && iy >= 0 && iy < rowCount
}