import scala.collection.mutable

class Table(rowCount: Int, columnCount: Int) {
  val data: mutable.ArrayBuffer[Cell] = mutable.ArrayBuffer.fill(rowCount * columnCount)(new EmptyCell)

  def getCell(row: Int, column: Int): Option[Cell] = {
    if (0 <= row && row < rowCount && 0 <= column && column < columnCount) {
      val index = row * columnCount + column
      Some(data(index))
    }
    else None
  }
  def setCell(row: Int, column: Int, cell: Cell): Unit = {
    if (0 <= row && row < rowCount && 0 <= column && column < columnCount) {
      val index = row * columnCount + column
      data(index) = cell
    }
  }
}
